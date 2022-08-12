module FloraWeb.Server where

import Colourista.IO (blueMessage)
import Control.Exception (bracket)
import Control.Monad (void, when)
import Data.Maybe (isJust)
import qualified Data.Pool as Pool
import Data.Text.Display (display)
import Effectful
import Log (Logger)
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

import qualified Control.Exception.Safe as Safe
import qualified Database.PostgreSQL.Simple as PG
import Effectful.Concurrent
import Effectful.Reader.Static (runReader, withReader)
import Effectful.Servant (effToHandler)
import Effectful.Time (Time, runCurrentTimeIO)
import qualified Network.HTTP.Client as HTTP
import qualified OddJobs.Endpoints as OddJobs
import OddJobs.Job (startJobRunner)
import qualified OddJobs.Types as OddJobs

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Effectful.Dispatch.Static
import Effectful.PostgreSQL.Transact.Effect (runDB)
import Flora.Environment (DeploymentEnv, FloraEnv (..), LoggingEnv (..), getFloraEnv)
import Flora.Environment.Config (FloraConfig (..))
import qualified Flora.Environment.OddJobs as OddJobs
import qualified Flora.OddJobs as OddJobs
import Flora.OddJobs.Types (JobsRunnerEnv (..))
import FloraWeb.Autoreload (AutoreloadRoute)
import qualified FloraWeb.Autoreload as Autoreload
import FloraWeb.Routes
import qualified FloraWeb.Routes.Pages as Pages
import FloraWeb.Server.Auth (FloraAuthContext, authHandler, runVisitorSession)
import FloraWeb.Server.Logging (runLog)
import qualified FloraWeb.Server.Logging as Logging
import FloraWeb.Server.Metrics
import qualified FloraWeb.Server.Pages as Pages
import FloraWeb.Server.Tracing
import FloraWeb.Types

runFlora :: IO ()
runFlora = bracket (runEff getFloraEnv) (runEff . shutdownFlora) $ \env -> runEff . runCurrentTimeIO . runConcurrent $ do
  let baseURL = "http://localhost:" <> display (env ^. #httpPort)
  liftIO $ blueMessage $ "🌺 Starting Flora server on " <> baseURL
  liftIO $ when (isJust $ env ^. (#logging % #sentryDSN)) (blueMessage "📋 Connected to Sentry endpoint")
  liftIO $ when (env ^. (#logging % #prometheusEnabled)) $ do
    blueMessage $ "📋 Service Prometheus metrics on " <> baseURL <> "/metrics"
    void $ Prometheus.register ghcMetrics
    void $ Prometheus.register procMetrics
  let withLogger = Logging.makeLogger (env ^. #logging ^. #logger)
  withLogger $ \appLogger ->
    runServer appLogger env

shutdownFlora :: FloraEnv -> Eff '[IOE] ()
shutdownFlora env =
  liftIO $
    Pool.destroyAllResources (env ^. #pool)

logException ::
  DeploymentEnv ->
  Logger ->
  Safe.SomeException ->
  IO ()
-- -> Eff '[IOE] ()
logException env logger exception =
  runEff
    . runCurrentTimeIO
    . Logging.runLog env logger
    $ Log.logAttention "odd-jobs runner crashed " (show exception)

runServer :: (Time :> es, Concurrent :> es, IOE :> es) => Logger -> FloraEnv -> Eff es ()
runServer appLogger floraEnv = do
  httpManager <- liftIO $ HTTP.newManager tlsManagerSettings
  jobRunnerPool <-
    liftIO $
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
    unsafeEff_ $
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
  liftIO
    $ runSettings warpSettings
    $ prometheusMiddleware (floraEnv ^. #environment) (floraEnv ^. #logging)
      . heartbeatMiddleware
      . loggingMiddleware
      . const
    $ server

mkServer :: Logger -> WebEnvStore -> FloraEnv -> OddJobs.UIConfig -> OddJobs.Env -> Application
mkServer logger webEnvStore floraEnv cfg jobsRunnerEnv = do
  genericServeTWithContext (naturalTransform (floraEnv ^. #environment) logger webEnvStore) (floraServer (floraEnv ^. #pool) cfg jobsRunnerEnv) (genAuthServerContext logger floraEnv)

floraServer :: Pool Connection -> OddJobs.UIConfig -> OddJobs.Env -> Routes (AsServerT Flora)
floraServer pool cfg jobsRunnerEnv =
  Routes
    { assets = serveDirectoryWebApp "./static"
    , pages = \sessionWithCookies ->
        hoistServerWithContext
          (Proxy @Pages.Routes)
          (Proxy @'[FloraAuthContext])
          ( \floraPage ->
              withReader (const sessionWithCookies)
                . runCurrentTimeIO
                . runDB pool
                . runVisitorSession
                $ floraPage
          )
          (Pages.server cfg jobsRunnerEnv)
    , autoreload =
        hoistServer
          (Proxy @AutoreloadRoute)
          ( \handler -> withReader (const ()) handler
          )
          Autoreload.server
    }

naturalTransform :: DeploymentEnv -> Logger -> WebEnvStore -> Flora a -> Handler a
naturalTransform deploymentEnv logger webEnvStore app =
  effToHandler
    . runLog deploymentEnv logger
    . runReader webEnvStore
    $ app

genAuthServerContext :: Logger -> FloraEnv -> Context '[FloraAuthContext]
genAuthServerContext logger floraEnv = authHandler logger floraEnv :. EmptyContext
