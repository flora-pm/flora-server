module FloraWeb.API.Routes where

import GHC.Generics
import Servant

import FloraWeb.API.Routes.Packages qualified as PackagesAPI

type Routes =
  "experimental"
    :> NamedRoutes Routes'

data Routes' mode = Routes'
  { packages :: mode :- "packages" :> PackagesAPI.API
  }
  deriving stock (Generic)
