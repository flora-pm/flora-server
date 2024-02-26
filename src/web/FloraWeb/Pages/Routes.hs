module FloraWeb.Pages.Routes where

import FloraWeb.Pages.Routes.Admin qualified as Admin
import FloraWeb.Pages.Routes.Categories qualified as Categories
import FloraWeb.Pages.Routes.Packages qualified as Packages
import FloraWeb.Pages.Routes.Search qualified as Search
import FloraWeb.Pages.Routes.Sessions qualified as Sessions
import FloraWeb.Pages.Routes.Settings qualified as Settings
import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { home :: mode :- AuthProtect "optional-cookie-auth" :> Get '[HTML] (Html ())
  , about :: mode :- AuthProtect "optional-cookie-auth" :> "about" :> Get '[HTML] (Html ())
  , admin :: mode :- "admin" :> Admin.Routes
  , sessions :: mode :- AuthProtect "optional-cookie-auth" :> "sessions" :> Sessions.Routes
  , packages :: mode :- "packages" :> Packages.Routes
  , categories :: mode :- AuthProtect "optional-cookie-auth" :> "categories" :> Categories.Routes
  , search :: mode :- AuthProtect "optional-cookie-auth" :> "search" :> Search.Routes
  , settings :: mode :- "settings" :> Settings.Routes
  , notFound :: mode :- AuthProtect "optional-cookie-auth" :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)
