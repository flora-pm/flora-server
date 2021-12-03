module FloraWeb.Server where

import Colourista.IO (blueMessage)
import Control.Monad
import Control.Monad.Reader (runReaderT)
import Data.Maybe
import qualified Data.Text as T
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Heartbeat (heartbeatMiddleware)
import Network.Wai.Middleware.Prometheus (PrometheusSettings (..), prometheus)
import Optics.Operators
import qualified Prometheus
import Prometheus.Metric.GHC (ghcMetrics)
import Servant
import Servant.API.Generic
import Servant.Server.Experimental.Auth
import Servant.Server.Generic

import Flora.Environment
import Flora.Model.User (User)
import Flora.Tracing
import FloraWeb.Server.Auth
import qualified FloraWeb.Server.Pages as Pages
import FloraWeb.Types

data Routes mode = Routes
  { assets :: mode :- "static" :> Raw
  , pages  :: mode :- Pages.Routes
  }
  deriving stock (Generic)

runFlora :: IO ()
runFlora = do
  env <- getFloraEnv
  blueMessage $ "🌺 Starting Flora server on http://localhost:" <> T.pack (show $ httpPort env)
  when (isJust $ env ^. #tracing ^. #sentryDSN) (blueMessage "📋 Connected to Sentry endpoint")
  Prometheus.register ghcMetrics
  runServer env

runServer :: FloraEnv -> IO ()
runServer floraEnv = withStdoutLogger $ \logger -> do
  let server = genericServeTWithContext (naturalTransform floraEnv) floraServer (genAuthServerContext floraEnv)
  let warpSettings = setPort (fromIntegral $ httpPort floraEnv ) $
                     setLogger logger $
                     setOnException (sentryOnException (floraEnv ^. #tracing))
                     defaultSettings
  let promMiddleware = prometheus $ PrometheusSettings ["metrics"] True True
  runSettings warpSettings $
    promMiddleware
    . heartbeatMiddleware
    $ server

floraServer :: Routes (AsServerT FloraM)
floraServer = Routes
  { assets = serveDirectoryWebApp "./static"
  , pages = Pages.server
  }

naturalTransform :: FloraEnv -> FloraM a -> Handler a
naturalTransform env app =
  runReaderT app env

genAuthServerContext :: FloraEnv -> Context (AuthHandler Request User ': '[])
genAuthServerContext floraEnv = authHandler floraEnv :. EmptyContext

