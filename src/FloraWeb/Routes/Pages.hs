module FloraWeb.Routes.Pages where

import Data.Text
import qualified FloraWeb.Routes.Pages.Admin as Admin
import qualified FloraWeb.Routes.Pages.Categories as Categories
import qualified FloraWeb.Routes.Pages.Packages as Packages
import qualified FloraWeb.Routes.Pages.Sessions as Sessions
import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { home :: mode :- Get '[HTML] (Html ())
  , about :: mode :- "about" :> Get '[HTML] (Html ())
  , admin :: mode :- "admin" :> Admin.Routes
  , sessions :: mode :- "sessions" :> Sessions.Routes
  , packages :: mode :- "packages" :> Packages.Routes
  , categories :: mode :- "categories" :> Categories.Routes
  , search :: mode :- "search" :> QueryParam "q" Text :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)
