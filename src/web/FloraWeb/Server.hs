module FloraWeb.Server where

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
import Effectful.Dispatch.Static
import Effectful.Error.Static (prettyCallStack, runErrorNoCallStack, runErrorWith)
import Effectful.Fail (runFailIO)
import Effectful.FileSystem
import Effectful.PostgreSQL.Transact.Effect (runDB)
import Effectful.Prometheus
import Effectful.Reader.Static (runReader)
import Effectful.Time (runTime)
import Effectful.Trace qualified as Trace
import GHC.Eventlog.Socket qualified as Socket
import Log (Logger)
import Log qualified
import Monitor.Tracing.Zipkin (Zipkin (..))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
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
import OddJobs.Endpoints qualified as OddJobs
import OddJobs.Job (startJobRunner)
import OddJobs.Types qualified as OddJobs
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
import Flora.Environment.Config (DeploymentEnv (..))
import Flora.Environment.Env
  ( BlobStoreImpl (..)
  , FeatureEnv (..)
  , FloraEnv (..)
  , MLTP (..)
  )
import Flora.Logging qualified as Logging
import Flora.Model.BlobStore.API
import Flora.Monad
import Flora.Tracing qualified as Tracing
import FloraJobs.Runner
import FloraJobs.Types (JobsRunnerEnv (..))
import FloraWeb.API.Routes qualified as API
import FloraWeb.API.Server qualified as API
import FloraWeb.Common.Auth
  ( OptionalAuthContext
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
   , StrictAuthContext
   , ErrorFormatters
   ]

runFlora :: IO ()
runFlora = do
  setBacktraceMechanismState HasCallStackBacktrace True
  secureMain $
    bracket
      (getFloraEnv & runFileSystem & runFailIO & runEff)
      (runEff . shutdownFlora)
      ( \env ->
          runEff . withUnliftStrategy (ConcUnlift Ephemeral Unlimited) . runTime . runConcurrent $ do
            let baseURL = "http://localhost:" <> display env.httpPort
            liftIO $ blueMessage $ "🌺 Starting Flora server on " <> baseURL
            liftIO $ when (isJust env.mltp.sentryDSN) (blueMessage "📋 Connecting to Sentry endpoint")
            liftIO $ do
              forM_ env.mltp.eventlogSocket Socket.start
              when (isJust env.mltp.eventlogSocket) (blueMessage "🔥 Sending live events to socket")
            liftIO $ when env.mltp.prometheusEnabled $ do
              blueMessage $ "🔥 Exposing Prometheus metrics at " <> baseURL <> "/metrics"
              void $ P.register P.ghcMetrics
              when (System.os == "linux") $ void $ P.register P.procMetrics
              void $ P.register (P.counter (P.Info "flora_imported_packages_total" "The number of imported packages"))
            liftIO $ when env.mltp.zipkinEnabled (blueMessage "🖊️ Connecting to Zipkin endpoint")
            liftIO $ when (env.environment == Development) (blueMessage "🔁 Live reloading enabled")
            let withLogger = Logging.makeLogger env.mltp.logger
            withLogger
              ( \appLogger ->
                  provideCallStack $ runServer appLogger env
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
logException env logger exception =
  runEff
    . runTime
    . Logging.runLog env logger
    $ Log.logAttention "odd-jobs runner crashed " (show exception)

runServer :: (Concurrent :> es, IOE :> es) => Logger -> FloraEnv -> FloraM es ()
runServer appLogger floraEnv = do
  httpManager <- liftIO $ HTTP.newManager tlsManagerSettings
  zipkin <- liftIO $ Tracing.newZipkin floraEnv.mltp.zipkinHost "flora-server"
  let runnerEnv = JobsRunnerEnv httpManager
  let oddjobsUiCfg = makeUIConfig floraEnv.config appLogger floraEnv.jobsPool
      oddJobsCfg =
        makeConfig
          runnerEnv
          floraEnv
          appLogger
          floraEnv.jobsPool

  void $
    forkIO $
      unsafeEff_ $
        Safe.withException (startJobRunner oddJobsCfg) (logException floraEnv.environment appLogger)
  loggingMiddleware <- Logging.runLog floraEnv.environment appLogger WaiLog.mkLogMiddleware
  let prometheusMiddleware =
        if floraEnv.mltp.prometheusEnabled
          then WaiMetrics.prometheus WaiMetrics.def
          else id
  oddJobsEnv <- OddJobs.mkEnv oddjobsUiCfg ("/admin/odd-jobs/" <>)
  let webEnv = WebEnv floraEnv
  webEnvStore <- liftIO $ newWebEnvStore webEnv
  ioref <- liftIO $ newIORef True
  let server = mkServer appLogger webEnvStore floraEnv oddjobsUiCfg oddJobsEnv zipkin ioref
  let warpSettings =
        setPort (fromIntegral floraEnv.httpPort) $
          setOnException
            ( handleExceptions
                appLogger
                floraEnv.environment
                floraEnv.mltp
            )
            defaultSettings
  liftIO
    $ runSettings warpSettings
    $ heartbeatMiddleware
      . loggingMiddleware
      . const
    $ P.prometheusMiddleware P.defaultMetrics (Proxy @ServerRoutes)
    $ prometheusMiddleware server

mkServer
  :: RequireCallStack
  => Logger
  -> WebEnvStore
  -> FloraEnv
  -> OddJobs.UIConfig
  -> OddJobs.Env
  -> Zipkin
  -> IORef Bool
  -> Application
mkServer logger webEnvStore floraEnv cfg jobsRunnerEnv zipkin ioref =
  serveWithContextT
    (Proxy @ServerRoutes)
    (genAuthServerContext logger floraEnv)
    (naturalTransform floraEnv logger webEnvStore zipkin)
    (floraServer cfg jobsRunnerEnv floraEnv.environment ioref)

floraServer
  :: RequireCallStack
  => OddJobs.UIConfig
  -> OddJobs.Env
  -> DeploymentEnv
  -> IORef Bool
  -> Routes (AsServerT FloraEff)
floraServer cfg jobsRunnerEnv environment ioref =
  Routes
    { assets = serveDirectoryWebApp "./static"
    , feed = Feed.server
    , openSearch = openSearchHandler
    , pages = \_ -> Pages.server cfg jobsRunnerEnv
    , api = API.apiServer
    , openApi = pure openApiHandler
    , docs = serveDirectoryWith docsBundler
    , livereload = LiveReload.livereloadHandler environment ioref
    , favicon = serveDirectoryWebApp "./static"
    }

naturalTransform
  :: RequireCallStack
  => FloraEnv
  -> Logger
  -> WebEnvStore
  -> Zipkin
  -> FloraEff a
  -> Handler a
naturalTransform floraEnv logger _webEnvStore zipkin app = do
  let runTrace =
        if floraEnv.environment == Production
          then Trace.runTrace zipkin.zipkinTracer
          else Trace.runNoTrace
  result <-
    liftIO $
      Right
        <$> app
          & runTrace
          & runDB floraEnv.pool
          & runTime
          & runReader floraEnv.features
          & ( case floraEnv.features.blobStoreImpl of
                Just (BlobStoreFS fp) -> runBlobStoreFS fp
                _ -> runBlobStorePure
            )
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
          & Logging.runLog floraEnv.environment logger
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
