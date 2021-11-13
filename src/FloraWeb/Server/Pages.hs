module FloraWeb.Server.Pages where

-- import Data.Text
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid
import Servant.Server.Generic

import FloraWeb.Templates
import qualified FloraWeb.Templates.Pages.Home as Home
import FloraWeb.Templates.Types
import FloraWeb.Types
import Lucid
-- import qualified FloraWeb.Server.Packages as Packages

type Routes = ToServantApi Routes'

data Routes' mode = Routes'
  { home :: mode :- Get '[HTML] (Html ())
  -- , packages :: mode :- "packages" :> Capture "package" Text :> Packages.Routes
  }
  deriving stock (Generic)

server :: ToServant Routes' (AsServerT FloraM)
server = genericServerT Routes'
  { home = homeHandler
  }

homeHandler :: FloraM (Html ())
homeHandler = do
  let assigns = emptyAssigns
  render assigns Home.show
