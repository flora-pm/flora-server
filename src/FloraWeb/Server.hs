module FloraWeb.Server where

import Colourista.IO (blueMessage)
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Data.Text.Display
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Heartbeat (heartbeatMiddleware)
import Optics.Operators
import qualified Prometheus
import Prometheus.Metric.GHC (ghcMetrics)
import Prometheus.Metric.Proc
import Servant hiding (Header, respond)
import Servant.Server.Generic

import Flora.Environment (FloraEnv (..), LoggingEnv (..), getFloraEnv)
import FloraWeb.Routes
import qualified FloraWeb.Routes.Pages as Pages
import FloraWeb.Server.Auth
import FloraWeb.Server.Logging.Metrics
import FloraWeb.Server.Logging.Tracing
import qualified FloraWeb.Server.Pages as Pages
import FloraWeb.Types

runFlora :: IO ()
runFlora = do
  env <- getFloraEnv
  let baseURL = "http://localhost:" <> display (httpPort env)
  blueMessage $ "🌺 Starting Flora server on " <> baseURL
  when (isJust $ env ^. #logging ^. #sentryDSN) (blueMessage "📋 Connected to Sentry endpoint")
  when (env ^. #logging ^. #prometheusEnabled) $ do
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
        (withReaderT (const sessionWithCookies)) -- Headers have been removed at this point, change it back if needed.
        Pages.server
  }

naturalTransform :: WebEnvStore -> FloraM a -> Handler a
naturalTransform webEnvStore app = do
  runReaderT app webEnvStore

genAuthServerContext :: FloraEnv -> Context '[FloraAuthContext]
genAuthServerContext floraEnv = authHandler floraEnv :. EmptyContext
