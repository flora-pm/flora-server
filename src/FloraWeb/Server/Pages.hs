module FloraWeb.Server.Pages where

import Control.Monad.Reader
import Lucid
import Network.HTTP.Types
import Optics.Core
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid
import Servant.Server.Generic

import FloraWeb.Server.Auth
import qualified FloraWeb.Server.Pages.Packages as Packages
import qualified FloraWeb.Server.Pages.Sessions as Sessions
import FloraWeb.Templates
import FloraWeb.Templates.Error
import qualified FloraWeb.Templates.Pages.Home as Home
import FloraWeb.Templates.Types

type Routes = ToServantApi Routes'

data Routes' mode = Routes'
  { home     :: mode :- Get '[HTML] (Html ())
  , about    :: mode :- "about" :> Get '[HTML] (Html ())
  , admin    :: mode :- "admin" :> Get '[HTML] (Html ())
  , login    :: mode :- "login" :> Sessions.Routes
  , packages :: mode :- "packages" :> Packages.Routes
  }
  deriving stock (Generic)

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
  render defaultTemplateEnv Home.about

adminHandler :: FloraAdminM (Html ())
adminHandler = undefined
