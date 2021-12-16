module FloraWeb.Routes.Pages where

import qualified FloraWeb.Routes.Pages.Packages as Packages
import qualified FloraWeb.Routes.Pages.Sessions as Sessions
import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid

type Routes = ToServantApi Routes'

data Routes' mode = Routes'
  { home     :: mode :- Get '[HTML] (Html ())
  , about    :: mode :- "about" :> Get '[HTML] (Html ())
  , admin    :: mode :- "admin" :> Get '[HTML] (Html ())
  , sessions :: mode :- "sessions" :> Sessions.Routes
  , packages :: mode :- "packages" :> Packages.Routes
  }
  deriving stock (Generic)
