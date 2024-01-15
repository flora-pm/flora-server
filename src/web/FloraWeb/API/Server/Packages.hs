module FloraWeb.API.Server.Packages where

import Data.Function
import Data.Maybe (fromMaybe)
import Data.Vector qualified as Vector
import Distribution.Version (Version)
import Servant

import Flora.Model.Component.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import FloraWeb.API.Errors
import FloraWeb.API.Routes.Packages qualified as Packages
import FloraWeb.API.Routes.Packages.Types (PackageDTO, toPackageDTO)
import FloraWeb.Common.Guards
import FloraWeb.Types

packagesServer :: ServerT Packages.API FloraAPI
packagesServer =
  Packages.API'
    { withPackage = withPackageServer
    }

withPackageServer :: Namespace -> PackageName -> ServerT Packages.PackageAPI FloraAPI
withPackageServer namespace packageName =
  Packages.PackageAPI'
    { getPackage = getPackageHandler namespace packageName
    , getVersionedPackage = getVersionedPackageHandler namespace packageName
    }

getPackageHandler
  :: Namespace
  -> PackageName
  -> FloraAPI (PackageDTO 0)
getPackageHandler namespace packageName = do
  package <- guardThatPackageExists namespace packageName packageNotFound
  releases <- Query.getReleases package.packageId
  let latestRelease =
        releases
          & Vector.filter (\r -> not (fromMaybe False r.deprecated))
          & Vector.maximumBy (compare `on` (.version))
      version = latestRelease.version
  release <-
    guardThatReleaseExists package.packageId version $
      versionNotFound
        package.namespace
        package.name
  components <- Query.getCanonicalComponentByReleaseId release.releaseId
  pure $ toPackageDTO package release components

getVersionedPackageHandler
  :: Namespace
  -> PackageName
  -> Version
  -> FloraAPI (PackageDTO 0)
getVersionedPackageHandler namespace packageName version = do
  package <- guardThatPackageExists namespace packageName packageNotFound
  release <-
    guardThatReleaseExists package.packageId version $
      versionNotFound
        package.namespace
        package.name
  components <- Query.getCanonicalComponentByReleaseId release.releaseId
  pure $ toPackageDTO package release components
