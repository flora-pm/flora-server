module FloraWeb.API.Routes.Packages where

import Distribution.Version
import GHC.Generics
import Servant

import Flora.Model.Package
import FloraWeb.API.Routes.Packages.Types

type API = NamedRoutes API'

type GetPackage =
  Summary "Get information about a package"
    :> Description
         "This endpoint returns information about a package and its latest version"
    :> Get '[JSON] (PackageDTO 0)

type GetVersionedPackage =
  Summary "Get information about a package and version"
    :> Description
         "This endpoint returns information about a package and the specified version"
    :> Capture "version" Version
    :> Get '[JSON] (PackageDTO 0)

data API' mode = API'
  { withPackage
      :: mode
        :- Capture "namespace" Namespace
          :> Capture "packageName" PackageName
          :> PackageAPI
  }
  deriving (Generic)

type PackageAPI = NamedRoutes PackageAPI'

data PackageAPI' mode = PackageAPI'
  { getPackage :: mode :- GetPackage
  , getVersionedPackage :: mode :- GetVersionedPackage
  }
  deriving (Generic)
