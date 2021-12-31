module FloraWeb.Server.Pages where

import Control.Monad.Reader
import Lucid
import Network.HTTP.Types
import Optics.Core
import Servant.API.Generic
import Servant.Server.Generic

import FloraWeb.Routes.Pages
import FloraWeb.Server.Auth
import FloraWeb.Templates
import FloraWeb.Templates.Error
import qualified FloraWeb.Server.Pages.Packages as Packages
import qualified FloraWeb.Server.Pages.Sessions as Sessions
import qualified FloraWeb.Templates.Pages.Home as Home

server :: ToServant Routes' (AsServerT FloraPageM)
server = genericServerT Routes'
  { home = homeHandler
  , about = aboutHandler
  , admin = ensureUser adminHandler
  , login = Sessions.server
  , packages = Packages.server
  }

ensureUser :: FloraAdminM a -> FloraPageM a
ensureUser adminM = do
  Session{sessionId, mUser} <- ask
  case mUser of
    Nothing -> renderError forbidden403
    Just user ->
      withReaderT (\Session{floraEnv} -> ProtectedSession{..}) adminM

homeHandler :: FloraPageM (Html ())
homeHandler = do
  env <- ask
  let templateEnv = fromSession env
          & (#displayNavbarSearch .~ False)
  render templateEnv Home.show

aboutHandler :: FloraPageM (Html ())
aboutHandler = do
  session <- ask
  let (templateEnv :: TemplateEnv) = fromSession session
        & (#activeElements % #aboutNav .~ True)
  render templateEnv Home.about

adminHandler :: FloraAdminM (Html ())
adminHandler = undefined
