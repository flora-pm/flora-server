module FloraWeb.Server where

import Colourista.IO (blueMessage)
import Control.Monad.Reader (runReaderT)
import qualified Data.Text as T
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Prometheus (PrometheusSettings (..), prometheus)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant
import Servant.API.Generic
import Servant.Server.Experimental.Auth
import Servant.Server.Generic

import Flora.Environment
import Flora.Model.User (User)
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
  register ghcMetrics
  runServer env

runServer :: FloraEnv -> IO ()
runServer floraEnv  = runSettings warpSettings $
  promMiddleware server
  where
    server = genericServeTWithContext (naturalTransform floraEnv) floraServer (genAuthServerContext floraEnv)
    warpSettings = setPort (fromIntegral $ httpPort floraEnv ) defaultSettings
    promMiddleware = prometheus $ PrometheusSettings ["metrics"] True True

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
