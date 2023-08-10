module FloraWeb.API.Routes where

import FloraWeb.API.Routes.Packages qualified as PackagesAPI
import GHC.Generics
import Servant

type Routes =
  "experimental"
    :> NamedRoutes Routes'

data Routes' mode = Routes'
  { packages :: mode :- "packages" :> PackagesAPI.API
  }
  deriving stock (Generic)
