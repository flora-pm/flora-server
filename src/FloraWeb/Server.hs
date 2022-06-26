module FloraWeb.Server where

import Colourista.IO (blueMessage)
import Control.Exception (bracket)
import Control.Monad (void, when)
import Control.Monad.Reader
  ( MonadIO (liftIO)
  , ReaderT (runReaderT)
  , withReaderT
  )
import Data.Maybe (isJust)
import qualified Data.Pool as Pool
import Data.Text.Display (display)
import Log (Logger, defaultLogLevel)
import qualified Log
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setOnException
  , setPort
  )
import qualified Network.Wai.Log as WaiLog
import Network.Wai.Middleware.Heartbeat (heartbeatMiddleware)
import Optics.Core
import qualified Prometheus
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

import Control.Concurrent
import qualified Control.Exception.Safe as Safe
import qualified Database.PostgreSQL.Simple as PG
import Flora.Environment (DeploymentEnv, FloraEnv (..), LoggingEnv (..), getFloraEnv)
import Flora.Environment.Config (FloraConfig (..))
import qualified Flora.Environment.OddJobs as OddJobs
import qualified Flora.OddJobs as OddJobs
import Flora.OddJobs.Types (JobsRunnerEnv (..))
import FloraWeb.Autoreload (AutoreloadRoute)
import qualified FloraWeb.Autoreload as Autoreload
import FloraWeb.Routes
import qualified FloraWeb.Routes.Pages as Pages
import FloraWeb.Server.Auth (FloraAuthContext, authHandler)
import qualified FloraWeb.Server.Logging as Logging
import FloraWeb.Server.Metrics
import qualified FloraWeb.Server.Pages as Pages
import FloraWeb.Server.Tracing
import FloraWeb.Types
import qualified Network.HTTP.Client as HTTP
import qualified OddJobs.Endpoints as OddJobs
import OddJobs.Job (startJobRunner)
import qualified OddJobs.Types as OddJobs

runFlora :: IO ()
runFlora = bracket getFloraEnv shutdownFlora $ \env -> do
  let baseURL = "http://localhost:" <> display (env ^. #httpPort)
  blueMessage $ "🌺 Starting Flora server on " <> baseURL
  when (isJust $ env ^. (#logging % #sentryDSN)) (blueMessage "📋 Connected to Sentry endpoint")
  when (env ^. (#logging % #prometheusEnabled)) $ do
    blueMessage $ "📋 Service Prometheus metrics on " <> baseURL <> "/metrics"
    void $ Prometheus.register ghcMetrics
    void $ Prometheus.register procMetrics
  let withLogger = Logging.makeLogger (env ^. #logging ^. #logger)
  withLogger $ \appLogger ->
    runServer appLogger env

shutdownFlora :: FloraEnv -> IO ()
shutdownFlora env = do
  Pool.destroyAllResources (env ^. #pool)

logException :: DeploymentEnv -> Logger -> Safe.SomeException -> IO ()
logException env logger exception =
  Logging.runLog env logger $ Log.logAttention "odd-jobs runner crashed " (show exception)

runServer :: Logger -> FloraEnv -> IO ()
runServer appLogger floraEnv = do
  httpManager <- HTTP.newManager tlsManagerSettings
  jobRunnerPool <-
    Pool.newPool $
      Pool.PoolConfig
        { createResource = PG.connect (floraEnv ^. #config % #connectInfo)
        , freeResource = PG.close
        , poolCacheTTL = 10
        , poolMaxResources = 10
        }
  let runnerEnv = JobsRunnerEnv httpManager
  let oddjobsUiCfg = OddJobs.makeUIConfig (floraEnv ^. #config) appLogger jobRunnerPool
      oddJobsCfg =
        OddJobs.makeConfig
          runnerEnv
          (floraEnv ^. #config)
          appLogger
          (floraEnv ^. #pool)
          OddJobs.runner

  forkIO $
    Safe.withException (startJobRunner oddJobsCfg) (logException (floraEnv ^. #environment) appLogger)
  loggingMiddleware <- Logging.runLog (floraEnv ^. #environment) appLogger WaiLog.mkLogMiddleware
  oddJobsEnv <- OddJobs.mkEnv oddjobsUiCfg ("/admin/odd-jobs/" <>)
  let webEnv = WebEnv floraEnv
  webEnvStore <- liftIO $ newWebEnvStore webEnv
  let server = mkServer appLogger webEnvStore floraEnv oddjobsUiCfg oddJobsEnv
  let warpSettings =
        setPort (fromIntegral $ floraEnv ^. #httpPort) $
          setOnException
            ( onException
                appLogger
                (floraEnv ^. #environment)
                (floraEnv ^. #logging)
            )
            defaultSettings
  runSettings warpSettings
    $ prometheusMiddleware (floraEnv ^. #environment) (floraEnv ^. #logging)
      . heartbeatMiddleware
      . loggingMiddleware
      . const
    $ server

mkServer :: Logger -> WebEnvStore -> FloraEnv -> OddJobs.UIConfig -> OddJobs.Env -> Application
mkServer logger webEnvStore floraEnv cfg jobsRunnerEnv = do
  genericServeTWithContext (naturalTransform logger webEnvStore) (floraServer cfg jobsRunnerEnv) (genAuthServerContext logger floraEnv)

floraServer :: OddJobs.UIConfig -> OddJobs.Env -> Routes (AsServerT FloraM)
floraServer cfg jobsRunnerEnv =
  Routes
    { assets = serveDirectoryWebApp "./static"
    , pages = \sessionWithCookies ->
        hoistServerWithContext
          (Proxy @Pages.Routes)
          (Proxy @'[FloraAuthContext])
          (\f -> withReaderT (const sessionWithCookies) f)
          (Pages.server cfg jobsRunnerEnv)
    , autoreload =
        hoistServer
          (Proxy @AutoreloadRoute)
          ( \handler -> withReaderT (const ()) handler
          )
          Autoreload.server
    }

naturalTransform :: Logger -> WebEnvStore -> FloraM a -> Handler a
naturalTransform logger webEnvStore app =
  Log.runLogT "flora" logger defaultLogLevel (runReaderT app webEnvStore)

genAuthServerContext :: Logger -> FloraEnv -> Context '[FloraAuthContext]
genAuthServerContext logger floraEnv = authHandler logger floraEnv :. EmptyContext
