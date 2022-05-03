module FloraWeb.Routes where

import Servant
import Servant.API.Generic

import FloraWeb.Autoreload (AutoreloadRoute)
import qualified FloraWeb.Routes.Pages as Pages

type ServerRoutes = NamedRoutes Routes

data Routes mode = Routes
  { assets :: mode :- "static" :> Raw
  , pages :: mode :- AuthProtect "optional-cookie-auth" :> Pages.Routes
  , autoreload :: mode :- AutoreloadRoute
  }
  deriving stock (Generic)
