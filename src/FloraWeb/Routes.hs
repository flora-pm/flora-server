{-# LANGUAGE CPP #-}
module FloraWeb.Routes where

import Servant
import Servant.API.Generic

#ifndef PROD
import FloraWeb.Autoreload (AutoreloadRoute)
#endif

import qualified FloraWeb.Routes.Pages as Pages

type ServerRoutes = NamedRoutes Routes

data Routes mode = Routes
  { assets :: mode :- "static" :> Raw
  , pages :: mode :- AuthProtect "optional-cookie-auth" :> Pages.Routes
#ifndef PROD
  , autoreload :: mode :- AutoreloadRoute
#endif
  }
  deriving stock (Generic)
