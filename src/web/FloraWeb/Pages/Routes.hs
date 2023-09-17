module FloraWeb.Pages.Routes where

import FloraWeb.Pages.Routes.Admin qualified as Admin
import FloraWeb.Pages.Routes.Categories qualified as Categories
import FloraWeb.Pages.Routes.Packages qualified as Packages
import FloraWeb.Pages.Routes.Search qualified as Search
import FloraWeb.Pages.Routes.Sessions qualified as Sessions
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
  , notFound :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)
