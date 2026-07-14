module FloraWeb.Server where

import Arbiter.Servant qualified as ArbS
import Colourista.IO (blueMessage)
import Control.Exception (bracket)
import Control.Exception.Backtrace
import Control.Exception.Safe qualified as Safe
import Control.Monad (forM_, void, when)
import Control.Monad.Except qualified as Except
import Data.Aeson
import Data.IORef (IORef, newIORef)
import Data.Maybe (isJust)
import Data.OpenApi (OpenApi)
import Data.Pool qualified as Pool
import Data.Text.Display (display)
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static (prettyCallStack, runErrorNoCallStack, runErrorWith)
import Effectful.Fail (runFailIO)
import Effectful.FileSystem
import Effectful.Log
import Effectful.Log qualified as Log
import Effectful.Prometheus
import Effectful.Reader.Static (runReader)
import Effectful.Time (runTime)
import Effectful.Tracing.Effect
import Effectful.Tracing.Instrumentation.Servant (traceServantMiddleware)
import GHC.Eventlog.Socket qualified as Socket
import Log
import Network.HTTP.Types (notFound404)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setOnException
  , setPort
  )
import Network.Wai.Log qualified as WaiLog
import Network.Wai.Middleware.Heartbeat (heartbeatMiddleware)
import Network.Wai.Middleware.Prometheus qualified as WaiMetrics
import Optics.Core
import Prometheus qualified as P
import Prometheus.Metric.GHC qualified as P
import Prometheus.Metric.Proc qualified as P
import Prometheus.Servant qualified as P
import RequireCallStack
import Sel
import Servant
  ( Application
  , Context (..)
  , ErrorFormatters
  , Handler
  , NotFoundErrorFormatter
  , Proxy (Proxy)
  , ServerError (..)
  , defaultErrorFormatters
  , err404
  , notFoundErrorFormatter
  , serveDirectoryWebApp
  , serveDirectoryWith
  , serveWithContextT
  )
import Servant.OpenApi
import Servant.Server.Generic (AsServerT)
import System.Info qualified as System

import Flora.Environment (getFloraEnv)
import Flora.Environment.Config (DeploymentEnv (..), FloraConfig (..), toConnString)
import Flora.Environment.Env
  ( FeatureEnv (..)
  , FloraEnv (..)
  , MLTP (..)
  )
import Flora.Logging qualified as Logging
import Flora.Model.BlobStore.API
import Flora.Model.Job
import Flora.Monitoring (setGitHash)
import Flora.Tracing
import Flora.Tracing qualified as Tracing
import FloraWeb.API.Routes qualified as API
import FloraWeb.API.Server qualified as API
import FloraWeb.Common.Auth
  ( AdminAuthContext
  , OptionalAuthContext
  , StrictAuthContext
  , adminAuthHandler
  , optionalAuthHandler
  , strictAuthHandler
  )
import FloraWeb.Common.OpenSearch
import FloraWeb.Common.Tracing
import FloraWeb.Embedded
import FloraWeb.Feed.Server qualified as Feed
import FloraWeb.LiveReload qualified as LiveReload
import FloraWeb.Pages.Server qualified as Pages
import FloraWeb.Pages.Templates (defaultTemplateEnv, defaultsToEnv)
import FloraWeb.Pages.Templates.Error (renderError)
import FloraWeb.Routes
import FloraWeb.Types
import Prometheus.Servant.HasEndpoint ()

type FloraAuthContext =
  '[ OptionalAuthContext
   , StrictAuthContext
   , AdminAuthContext
   , ErrorFormatters
   ]

runFlora :: FilePath -> IO ()
runFlora config = do
  setBacktraceMechanismState HasCallStackBacktrace True
  secureMain $
    bracket
      (getFloraEnv config & runFileSystem & runFailIO & runEff)
      (runEff . shutdownFlora)
      ( \env ->
          runEff . withUnliftStrategy (ConcUnlift Ephemeral Unlimited) . runTime . runConcurrent $ do
            let baseURL = "http://localhost:" <> display env.httpPort
            liftIO $ blueMessage $ "🌺 Starting Flora server on " <> baseURL
            liftIO $ when (isJust env.mltp.sentryDSN) (blueMessage "📋 Connecting to Sentry endpoint")
            liftIO $ do
              forM_ env.mltp.eventlogSocket Socket.start
              when (isJust env.mltp.eventlogSocket) (blueMessage "🔥 Sending live events to socket")
            when env.mltp.prometheusEnabled $ do
              liftIO $ blueMessage $ "🔥 Exposing Prometheus metrics at " <> baseURL <> "/metrics"
              runPrometheusMetrics env.metrics $ do
                void $ P.register P.ghcMetrics
                when (System.os == "linux") $ void $ P.register P.procMetrics
                setGitHash

            liftIO $ when env.mltp.zipkinEnabled (blueMessage "🖊️ Connecting to OpenTelemetry endpoint")
            liftIO $ when (env.environment == Development) (blueMessage "🔁 Live reloading enabled")
            traceRunner <- liftIO $ Tracing.newTraceRunner env.mltp.zipkinHost "flora-server"
            let withLogger = Logging.makeLogger "logs/flora-server.json" env.mltp.logger
            withLogger
              ( \appLogger ->
                  provideCallStack $ Tracing.runTraceRunner traceRunner $ runServer appLogger env traceRunner
              )
      )

shutdownFlora :: FloraEnv -> Eff '[IOE] ()
shutdownFlora env =
  liftIO $
    Pool.destroyAllResources env.pool

logException
  :: DeploymentEnv
  -> Logger
  -> Safe.SomeException
  -> IO ()
logException floraEnv logger exception =
  runEff
    . runTime
    . Log.runLog
      ("flora-server-" <> display floraEnv)
      logger
      defaultLogLevel
    $ Log.logAttention "Jobs runner crashed " (show exception)

runServer
  :: ( Concurrent :> es
     , IOE :> es
     , RequireCallStack
     , Tracer :> es
     )
  => Logger
  -> FloraEnv
  -> TraceRunner
  -> Eff es ()
runServer appLogger floraEnv traceRunner = do
  loggingMiddleware <-
    Log.runLog
      ("flora-server-" <> display floraEnv.environment)
      appLogger
      defaultLogLevel
      WaiLog.mkLogMiddleware
  let prometheusMiddleware =
        if floraEnv.mltp.prometheusEnabled
          then WaiMetrics.prometheus WaiMetrics.def
          else id
  let webEnv = WebEnv floraEnv
  webEnvStore <- liftIO $ newWebEnvStore webEnv
  ioref <- liftIO $ newIORef True
  let connectionInfo = floraEnv.config.connectionInfo
  arbiterConfig <-
    liftIO $
      ArbS.initArbiterServer
        (Proxy @JobQueues)
        (toConnString connectionInfo)
        "public"
  let server = mkServer arbiterConfig appLogger webEnvStore floraEnv ioref traceRunner
  let warpSettings =
        setPort (fromIntegral floraEnv.httpPort) $
          setOnException
            ( handleExceptions
                "flora-server"
                appLogger
                floraEnv.environment
                floraEnv.mltp
            )
            defaultSettings
  withEffToIO (ConcUnlift Persistent Unlimited) $ \runInIO ->
    runSettings warpSettings
      $ heartbeatMiddleware
        . loggingMiddleware
        . const
      $ P.prometheusMiddleware P.defaultMetrics (Proxy @ServerRoutes)
      $ traceServantMiddleware runInIO
      $ prometheusMiddleware server

mkServer
  :: RequireCallStack
  => ArbS.ArbiterServerConfig JobQueues
  -> Logger
  -> WebEnvStore
  -> FloraEnv
  -> IORef Bool
  -> TraceRunner
  -> Application
mkServer arbiterConfig logger webEnvStore floraEnv ioref traceRunner =
  serveWithContextT
    (Proxy @ServerRoutes)
    (genAuthServerContext logger floraEnv)
    (naturalTransform floraEnv logger webEnvStore traceRunner)
    (floraServer arbiterConfig floraEnv.environment ioref)

floraServer
  :: RequireCallStack
  => ArbS.ArbiterServerConfig JobQueues
  -> DeploymentEnv
  -> IORef Bool
  -> Routes (AsServerT FloraEff)
floraServer arbiterConfig environment ioref =
  Routes
    { assets = serveDirectoryWebApp "./static"
    , feed = Feed.server
    , openSearch = openSearchHandler
    , pages = const (Pages.server arbiterConfig)
    , api = API.apiServer
    , openApi = pure openApiHandler
    , docs = serveDirectoryWith docsBundler
    , livereload = LiveReload.livereloadHandler environment ioref
    }

naturalTransform
  :: RequireCallStack
  => FloraEnv
  -> Logger
  -> WebEnvStore
  -> TraceRunner
  -> FloraEff a
  -> Handler a
naturalTransform floraEnv logger _webEnvStore traceRunner app = do
  result <-
    liftIO $
      Right
        <$> app
          & Tracing.runTraceRunner traceRunner
          & runTime
          & runReader floraEnv.features
          & withBlobStore floraEnv.features
          & runErrorWith
            ( \callstack err -> do
                Log.logInfo "Server error" $
                  object
                    [ "error_headers" .= map show (errHeaders err)
                    , "error_http_code" .= errHTTPCode err
                    , "error_reason_phrase" .= errReasonPhrase err
                    , "exception" .= prettyCallStack callstack
                    ]
                pure . Left $ err
            )
          & Log.runLog ("flora-server-" <> display floraEnv.environment) logger defaultLogLevel
          & runConcurrent
          & runPrometheusMetrics floraEnv.metrics
          & runReader floraEnv
          & runEff
  either Except.throwError pure result

genAuthServerContext
  :: RequireCallStack
  => Logger
  -> FloraEnv
  -> Context FloraAuthContext
genAuthServerContext logger floraEnv =
  optionalAuthHandler logger floraEnv
    :. strictAuthHandler logger floraEnv
    :. adminAuthHandler logger floraEnv
    :. errorFormatters floraEnv
    :. EmptyContext

errorFormatters :: RequireCallStack => FloraEnv -> ErrorFormatters
errorFormatters floraEnv =
  defaultErrorFormatters{notFoundErrorFormatter = notFoundPage floraEnv}

notFoundPage :: RequireCallStack => FloraEnv -> NotFoundErrorFormatter
notFoundPage floraEnv _req =
  let result =
        runPureEff $
          runErrorNoCallStack $
            renderError (defaultsToEnv floraEnv defaultTemplateEnv) notFound404
   in case result of
        Left err -> err
        Right _ -> err404

openApiHandler :: OpenApi
openApiHandler =
  toOpenApi (Proxy @API.Routes)
    & #info
    % #title
    .~ "Flora API"
    & #info
    % #version
    .~ "v0"
    & #info
    % #description
    ?~ "Flora API Documentation"
