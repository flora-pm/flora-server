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
import qualified Data.UUID.V4 as UUID
import qualified GHC.IO.Unsafe as Unsafe
import Log (Logger, defaultLogLevel)
import qualified Log
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

import Flora.Environment (FloraEnv (..), LoggingEnv (..), getFloraEnv)
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

runFlora :: IO ()
runFlora = bracket getFloraEnv shutdownFlora $ \env -> do
  let baseURL = "http://localhost:" <> display (httpPort env)
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

runServer :: Logger -> FloraEnv -> IO ()
runServer appLogger floraEnv = do
  loggingMiddleware <- Logging.runLog floraEnv appLogger WaiLog.mkLogMiddleware
  let webEnv = WebEnv floraEnv
  webEnvStore <- liftIO $ newWebEnvStore webEnv
  let server = mkServer appLogger webEnvStore floraEnv
  let warpSettings =
        setPort (fromIntegral $ httpPort floraEnv) $
          setOnException
            ( sentryOnException
                (floraEnv ^. #environment)
                (floraEnv ^. #logging)
            )
            defaultSettings
  runSettings warpSettings $
    prometheusMiddleware (floraEnv ^. #environment) (floraEnv ^. #logging)
      . heartbeatMiddleware
      . loggingMiddleware
      . const
      $ server

mkServer :: Logger -> WebEnvStore -> FloraEnv -> Application
mkServer logger webEnvStore floraEnv =
  genericServeTWithContext (naturalTransform logger webEnvStore) floraServer (genAuthServerContext logger floraEnv)

floraServer :: Routes (AsServerT FloraM)
floraServer =
  Routes
    { assets = serveDirectoryWebApp "./static"
    , pages = \sessionWithCookies ->
        hoistServerWithContext
          (Proxy @Pages.Routes)
          (Proxy @'[FloraAuthContext])
          (\f -> withReaderT (const sessionWithCookies) f)
          Pages.server
    , autoreload =
        hoistServer
          (Proxy @AutoreloadRoute)
          ( \handler ->
              let connId = Unsafe.unsafePerformIO UUID.nextRandom
               in withReaderT (const connId) handler
          )
          Autoreload.server
    }
{-# NOINLINE floraServer #-}

naturalTransform :: Logger -> WebEnvStore -> FloraM a -> Handler a
naturalTransform logger webEnvStore app =
  Log.runLogT "flora" logger defaultLogLevel (runReaderT app webEnvStore)

genAuthServerContext :: Logger -> FloraEnv -> Context '[FloraAuthContext]
genAuthServerContext logger floraEnv = authHandler logger floraEnv :. EmptyContext
