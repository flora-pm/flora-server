module FloraWeb.API.Routes.Packages where

import Distribution.Version
import GHC.Generics
import Servant
import Data.Text (Text)
import Data.Vector (Vector)

import Flora.Model.Package
import FloraWeb.API.Routes.Packages.Types

type API = NamedRoutes API'

type GetPackage =
  Summary "Get information about a package"
    :> Description
         "Return information about a package and its latest version"
    :> Get '[JSON] (PackageDTO 0)

type GetVersionedPackage =
  Summary "Get information about a package and version"
    :> Description
         "Return information about a package and the specified version"
    :> Capture "version" Version
    :> Get '[JSON] (PackageDTO 0)

type GetPackageDependencies =
  Summary "Get dependencies of a package"
    :> Description
         "Return dependencies of a package"
    :> Capture "version" Version
    :> "dependencies"
    :> QueryFlag "transitive"
    :> Get '[JSON] (PackageDependenciesDTO 0)

type GetPackagesByPrefix =
  Summary "Get a list of packages"
    :> Description
         "Return a list of packages, given a prefix"
    :> Get '[JSON] (Vector PackageName)

data API' mode = API'
  { getPackagesByPrefix
      :: mode
        :- "search"
          :> Capture "packageNamePrefix" Text
          :> GetPackagesByPrefix
  , withPackage
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
  , getDependencies :: mode :- GetPackageDependencies
  }
  deriving (Generic)
