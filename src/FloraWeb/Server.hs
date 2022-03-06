module FloraWeb.Server where

import Colourista.IO (blueMessage)
import Control.Monad (void, when)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT),
                             withReaderT)
import Data.Maybe (isJust)
import Data.Text.Display (display)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger,
                                 setOnException, setPort)
import Network.Wai.Middleware.Heartbeat (heartbeatMiddleware)
import Optics.Core
import qualified Prometheus
import Prometheus.Metric.GHC (ghcMetrics)
import Prometheus.Metric.Proc (procMetrics)
import Servant (Application, Context (..), Handler,
                HasServer (hoistServerWithContext), Proxy (Proxy),
                serveDirectoryWebApp)
import Servant.Server.Generic (AsServerT, genericServeTWithContext)

import Control.Exception (bracket)
import qualified Data.Pool as Pool
import Flora.Environment (FloraEnv (..), LoggingEnv (..), getFloraEnv)
import FloraWeb.Routes
import qualified FloraWeb.Routes.Pages as Pages
import FloraWeb.Server.Auth (FloraAuthContext, authHandler)
import FloraWeb.Server.Logging.Metrics
import FloraWeb.Server.Logging.Tracing
import qualified FloraWeb.Server.Pages as Pages
import FloraWeb.Types
import Log (Logger, defaultLogLevel)
import qualified Log
import qualified Log.Backend.StandardOutput as Log
import qualified Network.Wai.Logger as WAI

runFlora :: IO ()
runFlora = bracket getFloraEnv shutdownFlora $ \env -> do
  let baseURL = "http://localhost:" <> display (httpPort env)
  blueMessage $ "🌺 Starting Flora server on " <> baseURL
  when (isJust $ env ^. (#logging % #sentryDSN)) (blueMessage "📋 Connected to Sentry endpoint")
  when (env ^. (#logging % #prometheusEnabled)) $ do
    blueMessage $ "📋 Service Prometheus metrics on " <> baseURL <> "/metrics"
    Prometheus.register ghcMetrics
    void $ Prometheus.register procMetrics
  runServer env

shutdownFlora :: FloraEnv -> IO ()
shutdownFlora env = do
  Pool.destroyAllResources (env ^. #pool)

runServer :: FloraEnv -> IO ()
runServer floraEnv = WAI.withStdoutLogger $ \waiLogger -> Log.withStdOutLogger $ \appLogger -> do
  let webEnv = WebEnv floraEnv
  webEnvStore  <- liftIO $ newWebEnvStore webEnv
  let server = mkServer appLogger webEnvStore floraEnv
  let warpSettings = setPort (fromIntegral $ httpPort floraEnv) $
                     setLogger waiLogger $
                     setOnException (sentryOnException (floraEnv ^. #environment)
                                                       (floraEnv ^. #logging))
                     defaultSettings
  runSettings warpSettings $
    prometheusMiddleware (floraEnv ^. #environment) (floraEnv ^. #logging)
    . heartbeatMiddleware
    $ server

mkServer :: Logger -> WebEnvStore -> FloraEnv -> Application
mkServer logger webEnvStore floraEnv =
  genericServeTWithContext (naturalTransform logger webEnvStore ) (floraServer logger) (genAuthServerContext logger floraEnv)

floraServer :: Logger -> Routes (AsServerT FloraM)
floraServer _logger = Routes
  { assets = serveDirectoryWebApp "./static"
  , pages = \sessionWithCookies ->
      hoistServerWithContext
        (Proxy @Pages.Routes)
        (Proxy @'[FloraAuthContext])
        (\f -> withReaderT (const sessionWithCookies) f)
        Pages.server
  }

naturalTransform :: Logger -> WebEnvStore -> FloraM a -> Handler a
naturalTransform logger webEnvStore app =
  Log.runLogT "flora" logger defaultLogLevel (runReaderT app webEnvStore)

genAuthServerContext :: Logger ->  FloraEnv -> Context '[FloraAuthContext]
genAuthServerContext logger floraEnv = authHandler logger floraEnv :. EmptyContext
