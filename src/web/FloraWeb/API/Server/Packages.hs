module FloraWeb.API.Server.Packages where

import Data.Function
import Data.Maybe (fromMaybe)
import Data.Vector qualified as Vector
import Distribution.Version (Version)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Trace
import Servant hiding ((:>))

import Flora.Model.Component.Query qualified as Query
import Flora.Model.Package.Guard
import Flora.Model.Package.Types
import Flora.Model.Release.Guard (guardThatReleaseExists)
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import FloraWeb.API.Errors
import FloraWeb.API.Routes.Packages qualified as Packages
import FloraWeb.API.Routes.Packages.Types (PackageDTO, toPackageDTO)
import FloraWeb.Types

packagesServer :: ServerT Packages.API FloraEff
packagesServer =
  Packages.API'
    { withPackage = withPackageServer
    }

withPackageServer :: Namespace -> PackageName -> ServerT Packages.PackageAPI FloraEff
withPackageServer namespace packageName =
  Packages.PackageAPI'
    { getPackage = getPackageHandler namespace packageName
    , getVersionedPackage = getVersionedPackageHandler namespace packageName
    }

getPackageHandler
  :: ( DB :> es
     , Error ServerError :> es
     , Trace :> es
     )
  => Namespace
  -> PackageName
  -> (Eff es) (PackageDTO 0)
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
  components <- Query.getComponentsByReleaseId release.releaseId
  pure $ toPackageDTO package release components

getVersionedPackageHandler
  :: ( DB :> es
     , Error ServerError :> es
     , Trace :> es
     )
  => Namespace
  -> PackageName
  -> Version
  -> (Eff es) (PackageDTO 0)
getVersionedPackageHandler namespace packageName version = do
  package <- guardThatPackageExists namespace packageName packageNotFound
  release <-
    guardThatReleaseExists package.packageId version $
      versionNotFound
        package.namespace
        package.name
  components <- Query.getComponentsByReleaseId release.releaseId
  pure $ toPackageDTO package release components
