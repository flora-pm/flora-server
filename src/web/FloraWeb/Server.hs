module FloraWeb.Server where

import Colourista.IO (blueMessage)
import Control.Exception (bracket)
import Control.Exception.Safe qualified as Safe
import Control.Monad (void, when)
import Control.Monad.Except qualified as Except
import Data.IORef (IORef, newIORef)
import Data.Maybe (isJust)
import Data.OpenApi (OpenApi)
import Data.Pool qualified as Pool
import Data.Text.Display (display)
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Static
import Effectful.Error.Static (runErrorNoCallStack, runErrorWith)
import Effectful.Fail (runFailIO)
import Effectful.PostgreSQL.Transact.Effect (runDB)
import Effectful.Reader.Static (runReader)
import Effectful.Time (runTime)
import Effectful.Trace qualified as Trace
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
import Network.Wai.Middleware.Prometheus qualified as P
import OddJobs.Endpoints qualified as OddJobs
import OddJobs.Job (startJobRunner)
import OddJobs.Types qualified as OddJobs
import Optics.Core
import Prometheus qualified as P
import Prometheus.Metric.GHC qualified as P
import Prometheus.Metric.Proc qualified as P
import Sel
import Servant
  ( Application
  , Context (..)
  , ErrorFormatters
  , Handler
  , NotFoundErrorFormatter
  , Proxy (Proxy)
  , defaultErrorFormatters
  , err404
  , notFoundErrorFormatter
  , serveDirectoryWebApp
  , serveDirectoryWith
  , serveWithContextT
  )
import Servant.OpenApi
import Servant.Server.Generic (AsServerT)

import Flora.Environment (getFloraEnv)
import Flora.Environment.Config (Assets, DeploymentEnv (..))
import Flora.Environment.Env
  ( BlobStoreImpl (..)
  , FeatureEnv (..)
  , FloraEnv (..)
  , MLTP (..)
  )
import Flora.Logging qualified as Logging
import Flora.Model.BlobStore.API
import Flora.Tracing qualified as Tracing
import FloraJobs.Runner (runner)
import FloraJobs.Types (JobsRunnerEnv (..), makeConfig, makeUIConfig)
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
import FloraWeb.LiveReload qualified as LiveReload
import FloraWeb.Pages.Server qualified as Pages
import FloraWeb.Pages.Templates (defaultTemplateEnv, defaultsToEnv)
import FloraWeb.Pages.Templates.Error (renderError)
import FloraWeb.Routes
import FloraWeb.Types

type FloraAuthContext =
  '[ OptionalAuthContext
   , StrictAuthContext
   , StrictAuthContext
   , ErrorFormatters
   ]

runFlora :: IO ()
runFlora =
  secureMain $
    bracket
      (getFloraEnv & runFailIO & runEff)
      (runEff . shutdownFlora)
      ( \env ->
          runEff . withUnliftStrategy (ConcUnlift Ephemeral Unlimited) . runTime . runConcurrent $ do
            let baseURL = "http://localhost:" <> display env.httpPort
            liftIO $ blueMessage $ "🌺 Starting Flora server on " <> baseURL
            liftIO $ when (isJust env.mltp.sentryDSN) (blueMessage "📋 Connecting to Sentry endpoint")
            liftIO $ when env.mltp.prometheusEnabled $ do
              blueMessage $ "🔥 Exposing Prometheus metrics at " <> baseURL <> "/metrics"
              void $ P.register P.ghcMetrics
              void $ P.register P.procMetrics
              void $ P.register (P.counter (P.Info "flora_imported_packages_total" "The number of imported packages"))
            liftIO $ when env.mltp.zipkinEnabled (blueMessage "🖊️ Connecting to Zipkin endpoint")
            liftIO $ when (env.environment == Development) (blueMessage "🔁 Live reloading enabled")
            let withLogger = Logging.makeLogger env.mltp.logger
            withLogger
              ( \appLogger ->
                  runServer appLogger env
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

runServer :: (Concurrent :> es, IOE :> es) => Logger -> FloraEnv -> Eff es ()
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
          runner

  void $
    forkIO $
      unsafeEff_ $
        Safe.withException (startJobRunner oddJobsCfg) (logException floraEnv.environment appLogger)
  loggingMiddleware <- Logging.runLog floraEnv.environment appLogger WaiLog.mkLogMiddleware
  let prometheusMiddleware =
        if floraEnv.mltp.prometheusEnabled
          then P.prometheus P.def
          else id
  oddJobsEnv <- OddJobs.mkEnv oddjobsUiCfg ("/admin/odd-jobs/" <>)
  let webEnv = WebEnv floraEnv
  webEnvStore <- liftIO $ newWebEnvStore webEnv
  ioref <- liftIO $ newIORef True
  let server = mkServer appLogger webEnvStore floraEnv oddjobsUiCfg oddJobsEnv zipkin ioref
  let warpSettings =
        setPort (fromIntegral floraEnv.httpPort) $
          setOnException
            ( onException
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
    $ prometheusMiddleware server

mkServer
  :: Logger
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
  :: OddJobs.UIConfig
  -> OddJobs.Env
  -> DeploymentEnv
  -> IORef Bool
  -> Routes (AsServerT FloraEff)
floraServer cfg jobsRunnerEnv environment ioref =
  Routes
    { assets = serveDirectoryWebApp "./static"
    , openSearch = openSearchHandler
    , pages = \_ -> Pages.server cfg jobsRunnerEnv
    , api = API.apiServer
    , openApi = pure openApiHandler
    , docs = serveDirectoryWith docsBundler
    , livereload = LiveReload.livereloadHandler environment ioref
    }

naturalTransform :: FloraEnv -> Logger -> WebEnvStore -> Zipkin -> FloraEff a -> Handler a
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
          & Logging.runLog floraEnv.environment logger
          & runErrorWith (\_callstack err -> pure $ Left err)
          & runConcurrent
          & runEff
  either Except.throwError pure result

genAuthServerContext :: Logger -> FloraEnv -> Context FloraAuthContext
genAuthServerContext logger floraEnv =
  optionalAuthHandler logger floraEnv
    :. strictAuthHandler logger floraEnv
    :. adminAuthHandler logger floraEnv
    :. errorFormatters floraEnv.assets
    :. EmptyContext

errorFormatters :: Assets -> ErrorFormatters
errorFormatters assets =
  defaultErrorFormatters{notFoundErrorFormatter = notFoundPage assets}

notFoundPage :: Assets -> NotFoundErrorFormatter
notFoundPage assets _req =
  let result =
        runPureEff $
          runErrorNoCallStack $
            renderError (defaultsToEnv assets defaultTemplateEnv) notFound404
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
