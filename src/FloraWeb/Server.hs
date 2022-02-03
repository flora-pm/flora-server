module FloraWeb.Server where

import Colourista.IO (blueMessage)
import Control.Monad (void, when)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT),
                             withReaderT)
import Data.Maybe (isJust)
import Data.Text.Display (display)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger,
                                 setOnException, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Heartbeat (heartbeatMiddleware)
import Optics.Core
import qualified Prometheus
import Prometheus.Metric.GHC (ghcMetrics)
import Prometheus.Metric.Proc (procMetrics)
import Servant (Context (..), Handler, HasServer (hoistServerWithContext),
                Proxy (Proxy), serveDirectoryWebApp)
import Servant.Server.Generic (AsServerT, genericServeTWithContext)

import Flora.Environment (FloraEnv (..), LoggingEnv (..), getFloraEnv)
import FloraWeb.Routes
import qualified FloraWeb.Routes.Pages as Pages
import FloraWeb.Server.Auth (FloraAuthContext, authHandler)
import FloraWeb.Server.Logging.Metrics
import FloraWeb.Server.Logging.Tracing
import qualified FloraWeb.Server.Pages as Pages
import FloraWeb.Types

runFlora :: IO ()
runFlora = do
  env <- getFloraEnv
  let baseURL = "http://localhost:" <> display (httpPort env)
  blueMessage $ "🌺 Starting Flora server on " <> baseURL
  when (isJust $ env ^. (#logging % #sentryDSN)) (blueMessage "📋 Connected to Sentry endpoint")
  when (env ^. (#logging % #prometheusEnabled)) $ do
    blueMessage $ "📋 Service Prometheus metrics on " <> baseURL <> "/metrics"
    Prometheus.register ghcMetrics
    void $ Prometheus.register procMetrics
  runServer env

runServer :: FloraEnv -> IO ()
runServer floraEnv = withStdoutLogger $ \logger -> do
  let webEnv = WebEnv floraEnv
  webEnvStore  <- liftIO $ newWebEnvStore webEnv
  let server = genericServeTWithContext
                 (naturalTransform webEnvStore) floraServer (genAuthServerContext floraEnv)
  let warpSettings = setPort (fromIntegral $ httpPort floraEnv ) $
                     setLogger logger $
                     setOnException (sentryOnException (floraEnv ^. #environment)
                                                       (floraEnv ^. #logging))
                     defaultSettings
  runSettings warpSettings $
    prometheusMiddleware (floraEnv ^. #environment) (floraEnv ^. #logging)
    . heartbeatMiddleware
    $ server

floraServer :: Routes (AsServerT FloraM)
floraServer = Routes
  { assets = serveDirectoryWebApp "./static"
  , pages = \sessionWithCookies ->
      hoistServerWithContext
        (Proxy @Pages.Routes)
        (Proxy @'[FloraAuthContext])
        (withReaderT (const sessionWithCookies))
        Pages.server
  }

naturalTransform :: WebEnvStore -> FloraM a -> Handler a
naturalTransform webEnvStore app =
  runReaderT app webEnvStore

genAuthServerContext :: FloraEnv -> Context '[FloraAuthContext]
genAuthServerContext floraEnv = authHandler floraEnv :. EmptyContext
