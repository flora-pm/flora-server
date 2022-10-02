module FloraWeb.Server where

import Colourista.IO (blueMessage)
import Control.Exception (bracket)
import Control.Monad (void, when)
import Data.Maybe (isJust)
import Data.Pool qualified as Pool
import Data.Text.Display (display)
import Effectful
import Log (Logger)
import Log qualified
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setOnException
  , setPort
  )
import Network.Wai.Log qualified as WaiLog
import Network.Wai.Middleware.Heartbeat (heartbeatMiddleware)
import Prometheus qualified
import Prometheus.Metric.GHC (ghcMetrics)
import Prometheus.Metric.Proc (procMetrics)
import Servant
  ( Application
  , Context (..)
  , Handler
  , HasServer (hoistServerWithContext)
  , Proxy (Proxy)
  , hoistServer
  , serveDirectoryWebApp
  )
import Servant.Server.Generic (AsServerT, genericServeTWithContext)

import Control.Exception.Safe qualified as Safe
import Effectful.Concurrent
import Effectful.Reader.Static (runReader, withReader)
import Effectful.Servant (effToHandler)
import Effectful.Time (Time, runCurrentTimeIO)
import Network.HTTP.Client qualified as HTTP
import OddJobs.Endpoints qualified as OddJobs
import OddJobs.Job (startJobRunner)
import OddJobs.Types qualified as OddJobs

import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Effectful.Dispatch.Static
import Effectful.PostgreSQL.Transact.Effect (runDB)
import Flora.Environment (DeploymentEnv, FloraEnv (..), LoggingEnv (..), getFloraEnv)
import Flora.Environment.OddJobs qualified as OddJobs
import Flora.OddJobs qualified as OddJobs
import Flora.OddJobs.Types (JobsRunnerEnv (..))
import FloraWeb.Autoreload (AutoreloadRoute)
import FloraWeb.Autoreload qualified as Autoreload
import FloraWeb.Routes
import FloraWeb.Routes.Pages qualified as Pages
import FloraWeb.Server.Auth (FloraAuthContext, authHandler, requestID, runVisitorSession)
import FloraWeb.Server.Logging (runLog)
import FloraWeb.Server.Logging qualified as Logging
import FloraWeb.Server.Metrics
import FloraWeb.Server.OpenSearch
import FloraWeb.Server.Pages qualified as Pages
import FloraWeb.Server.Tracing
import FloraWeb.Types
import Servant.API (getResponse)

runFlora :: IO ()
runFlora = bracket (runEff getFloraEnv) (runEff . shutdownFlora) $ \env -> runEff . runCurrentTimeIO . runConcurrent $ do
  let baseURL = "http://localhost:" <> display (env.httpPort)
  liftIO $ blueMessage $ "🌺 Starting Flora server on " <> baseURL
  liftIO $ when (isJust $ env.logging.sentryDSN) (blueMessage "📋 Connected to Sentry endpoint")
  liftIO $ when env.logging.prometheusEnabled $ do
    blueMessage $ "📋 Service Prometheus metrics on " <> baseURL <> "/metrics"
    void $ Prometheus.register ghcMetrics
    void $ Prometheus.register procMetrics
  let withLogger = Logging.makeLogger (env.logging.logger)
  withLogger $ \appLogger ->
    runServer appLogger env

shutdownFlora :: FloraEnv -> Eff '[IOE] ()
shutdownFlora env =
  liftIO $
    Pool.destroyAllResources (env.pool)

logException
  :: DeploymentEnv
  -> Logger
  -> Safe.SomeException
  -> IO ()
logException env logger exception =
  runEff
    . runCurrentTimeIO
    . Logging.runLog env logger
    $ Log.logAttention "odd-jobs runner crashed " (show exception)

runServer :: (Time :> es, Concurrent :> es, IOE :> es) => Logger -> FloraEnv -> Eff es ()
runServer appLogger floraEnv = do
  httpManager <- liftIO $ HTTP.newManager tlsManagerSettings
  let runnerEnv = JobsRunnerEnv httpManager
  let oddjobsUiCfg = OddJobs.makeUIConfig (floraEnv.config) appLogger (floraEnv.pool)
      oddJobsCfg =
        OddJobs.makeConfig
          runnerEnv
          (floraEnv.config)
          appLogger
          (floraEnv.jobsPool)
          OddJobs.runner

  forkIO $
    unsafeEff_ $
      Safe.withException (startJobRunner oddJobsCfg) (logException (floraEnv.environment) appLogger)
  loggingMiddleware <- Logging.runLog (floraEnv.environment) appLogger WaiLog.mkLogMiddleware
  oddJobsEnv <- OddJobs.mkEnv oddjobsUiCfg ("/admin/odd-jobs/" <>)
  let webEnv = WebEnv floraEnv
  webEnvStore <- liftIO $ newWebEnvStore webEnv
  let server = mkServer appLogger webEnvStore floraEnv oddjobsUiCfg oddJobsEnv
  let warpSettings =
        setPort (fromIntegral $ floraEnv.httpPort) $
          setOnException
            ( onException
                appLogger
                (floraEnv.environment)
                (floraEnv.logging)
            )
            defaultSettings
  liftIO
    $ runSettings warpSettings
    $ prometheusMiddleware (floraEnv.environment) (floraEnv.logging)
      . heartbeatMiddleware
      . loggingMiddleware
      . const
    $ server

mkServer :: Logger -> WebEnvStore -> FloraEnv -> OddJobs.UIConfig -> OddJobs.Env -> Application
mkServer logger webEnvStore floraEnv cfg jobsRunnerEnv = do
  genericServeTWithContext (naturalTransform (floraEnv.environment) logger webEnvStore) (floraServer (floraEnv.pool) cfg jobsRunnerEnv) (genAuthServerContext logger floraEnv)

floraServer :: Pool Connection -> OddJobs.UIConfig -> OddJobs.Env -> Routes (AsServerT Flora)
floraServer pool cfg jobsRunnerEnv =
  Routes
    { assets = serveDirectoryWebApp "./static"
    , openSearch = openSearchHandler
    , pages = \sessionWithCookies ->
        hoistServerWithContext
          (Proxy @Pages.Routes)
          (Proxy @'[FloraAuthContext])
          ( \floraPage ->
              floraPage
                & runVisitorSession
                & runDB pool
                & Log.localData [("request_id", Aeson.String $ requestID . getResponse $ sessionWithCookies)]
                & runCurrentTimeIO
                & withReader (const sessionWithCookies)
          )
          (Pages.server cfg jobsRunnerEnv)
    , autoreload =
        hoistServer
          (Proxy @AutoreloadRoute)
          ( \handler ->
              withReader (const ()) handler
          )
          Autoreload.server
    }

naturalTransform :: DeploymentEnv -> Logger -> WebEnvStore -> Flora a -> Handler a
naturalTransform deploymentEnv logger webEnvStore app =
  app
    & runReader webEnvStore
    & runLog deploymentEnv logger
    & effToHandler

genAuthServerContext :: Logger -> FloraEnv -> Context '[FloraAuthContext]
genAuthServerContext logger floraEnv = authHandler logger floraEnv :. EmptyContext
