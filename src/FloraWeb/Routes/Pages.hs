module FloraWeb.Routes.Pages where

import qualified FloraWeb.Routes.Pages.Packages as Packages
import qualified FloraWeb.Routes.Pages.Sessions as Sessions
import qualified FloraWeb.Routes.Pages.Admin as Admin
import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { home     :: mode :- Get '[HTML] (Html ())
  , about    :: mode :- "about" :> Get '[HTML] (Html ())
  , admin    :: mode :- "admin" :> Admin.Routes
  , sessions :: mode :- "sessions" :> Sessions.Routes
  , packages :: mode :- "packages" :> Packages.Routes
  }
  deriving stock (Generic)
