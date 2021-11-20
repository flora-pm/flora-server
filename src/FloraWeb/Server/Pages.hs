module FloraWeb.Server.Pages where

import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid
import Servant.Server.Generic

import qualified FloraWeb.Server.Packages as Packages
import FloraWeb.Templates
import qualified FloraWeb.Templates.Pages.Home as Home
import FloraWeb.Templates.Types
import FloraWeb.Types

type Routes = ToServantApi Routes'

data Routes' mode = Routes'
  { home     :: mode :- Get '[HTML] (Html ())
  , packages :: mode :- "packages" :> Packages.Routes
  }
  deriving stock (Generic)

server :: ToServant Routes' (AsServerT FloraM)
server = genericServerT Routes'
  { home = homeHandler
  , packages = Packages.server
  }

homeHandler :: FloraM (Html ())
homeHandler = do
  let assigns = emptyAssigns
  render assigns Home.show
