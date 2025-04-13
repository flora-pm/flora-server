module FloraWeb.API.Server.Packages where

import Data.Function
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Version (Version)
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Time (Time)
import Effectful.Trace
import Servant hiding ((:>))

import Flora.Model.Component.Query qualified as Query
import Flora.Model.Component.Types
import Flora.Model.Package.Guard
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Model.Release.Guard (guardThatReleaseExists)
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Search (searchPackageByName)
import FloraWeb.API.Errors
import FloraWeb.API.Routes.Packages qualified as Packages
import FloraWeb.API.Routes.Packages.Types
import FloraWeb.Types

packagesServer :: ServerT Packages.API FloraEff
packagesServer =
  Packages.API'
    { withPackage = withPackageServer
    , getPackagesByPrefix = getPackagesByPrefixHandler
    }

withPackageServer :: Namespace -> PackageName -> ServerT Packages.PackageAPI FloraEff
withPackageServer namespace packageName =
  Packages.PackageAPI'
    { getPackage = getPackageHandler namespace packageName
    , getVersionedPackage = getVersionedPackageHandler namespace packageName
    , getDependencies = getDependenciesHandler namespace packageName
    }

getDependenciesHandler
  :: Namespace
  -> PackageName
  -> Version
  -> Bool
  -> Eff RouteEffects (PackageDependenciesDTO 0)
getDependenciesHandler namespace packageName version transitive = do
  package <- guardThatPackageExists namespace packageName packageNotFound
  release <-
    guardThatReleaseExists package.packageId version $
      versionNotFound
        package.namespace
        package.name
  mComponent <- Query.getComponent release.releaseId (display packageName) Library
  case mComponent of
    Nothing -> error "lol"
    Just component ->
      if transitive
        then do
          transitiveDependencies <- Query.getTransitiveDependencies component.componentId
          let packageDependencies = transitiveDependencies
          pure $ PackageDependenciesDTO packageDependencies
        else do
          requirements <- Query.getRequirements package.name release.releaseId
          let dependencies = Vector.singleton $ PackageDependencies package.namespace package.name requirements
          pure $ PackageDependenciesDTO dependencies

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

getPackagesByPrefixHandler
  :: ( DB :> es
     , Log :> es
     , Time :> es
     )
  => Maybe Text
  -> Maybe Word
  -> Maybe Word
  -> (Eff es) (Vector PackageName)
getPackagesByPrefixHandler maybePackageName maybeOffset maybeLimit = do
  let packageName = fromMaybe mempty maybePackageName
  let offset = fromMaybe 0 maybeOffset
  let limit = fromMaybe 10 maybeLimit
  (_, packagesInfo) <- searchPackageByName (offset, limit) packageName
  pure
    (Vector.map (\p -> p.name) packagesInfo)

getVersionedPackageHandler
  :: ( DB :> es
     , Error ServerError :> es
     , IOE :> es
     , Trace :> es
     )
  => Namespace
  -> PackageName
  -> Version
  -> (Eff es) (PackageDTO 0)
getVersionedPackageHandler namespace packageName version = do
  liftIO $ print @String "LOOOOOOOOOOOOOOOOOOOOOL"
  package <- guardThatPackageExists namespace packageName packageNotFound
  release <-
    guardThatReleaseExists package.packageId version $
      versionNotFound
        package.namespace
        package.name
  components <- Query.getComponentsByReleaseId release.releaseId
  pure $ toPackageDTO package release components
