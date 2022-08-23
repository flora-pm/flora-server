module FloraWeb.Routes.Pages where

import FloraWeb.Routes.Pages.Admin qualified as Admin
import FloraWeb.Routes.Pages.Categories qualified as Categories
import FloraWeb.Routes.Pages.Packages qualified as Packages
import FloraWeb.Routes.Pages.Search qualified as Search
import FloraWeb.Routes.Pages.Sessions qualified as Sessions
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
  , search :: mode :- "search" :> Search.Routes
  }
  deriving stock (Generic)
