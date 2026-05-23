module FloraWeb.API.Server.Packages where

import Control.Applicative (asum)
import Data.Function
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Version (Version)
import Effectful (IOE, (:>))
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time (Time)
import Effectful.Trace
import RequireCallStack
import Servant hiding ((:>))

import Flora.Database
import Flora.Environment.Env
import Flora.Model.Component.Query qualified as Query
import Flora.Model.Component.Types
import Flora.Model.Package.Guard
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Model.Release.Guard (guardThatReleaseExists)
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Monad
import Flora.Search (searchPackageByName)
import FloraWeb.API.Errors
import FloraWeb.API.Routes.Packages qualified as Packages
import FloraWeb.API.Routes.Packages.Types
import FloraWeb.Types

packagesServer :: RequireCallStack => ServerT Packages.API FloraEff
packagesServer =
  Packages.API'
    { withPackage = withPackageServer
    , getPackagesByPrefix = getPackagesByPrefixHandler
    }

withPackageServer :: RequireCallStack => Namespace -> PackageName -> ServerT Packages.PackageAPI FloraEff
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
  -> FloraM RouteEffects (PackageDependenciesDTO 0)
getDependenciesHandler namespace packageName version transitive = do
  FloraEnv{pool} <- Reader.ask
  package <- withReadOnlyPool pool $ guardThatPackageExists namespace packageName packageNotFound
  release <-
    withReadOnlyPool pool $
      guardThatReleaseExists package.packageId version $
        versionNotFound
          package.namespace
          package.name

  mMainLibrary <- withReadOnlyPool pool $ Query.getComponent release.releaseId (display packageName) Library
  mMainExecutable <- withReadOnlyPool pool $ Query.getComponent release.releaseId (display packageName) Executable

  let componentToUse =
        asum
          [ mMainLibrary
          , mMainExecutable
          ]

  dependencies <- case componentToUse of
    Nothing -> pure mempty
    Just component ->
      if transitive
        then do
          withReadOnlyPool pool $ Query.getTransitiveDependencies component.componentId
        else do
          requirements <- withReadOnlyPool pool $ Query.getRequirements package.name release.releaseId
          pure $ Vector.singleton $ PackageDependencies package.namespace package.name requirements
  pure $ PackageDependenciesDTO dependencies

getPackageHandler
  :: ( Error ServerError :> es
     , IOE :> es
     , Reader FloraEnv :> es
     , Trace :> es
     )
  => Namespace
  -> PackageName
  -> (FloraM es) (PackageDTO 0)
getPackageHandler namespace packageName = do
  FloraEnv{pool} <- Reader.ask
  package <- withReadOnlyPool pool $ guardThatPackageExists namespace packageName packageNotFound
  releases <- withReadOnlyPool pool $ Query.getReleases package.packageId
  let latestRelease =
        releases
          & Vector.filter (\r -> not (fromMaybe False r.deprecated))
          & Vector.maximumBy (compare `on` (.version))
      version = latestRelease.version
  release <-
    withReadOnlyPool pool $
      guardThatReleaseExists package.packageId version $
        versionNotFound
          package.namespace
          package.name
  components <- withReadOnlyPool pool $ Query.getComponentsByReleaseId release.releaseId
  pure $ toPackageDTO package release components

getPackagesByPrefixHandler
  :: ( IOE :> es
     , Log :> es
     , Reader FloraEnv :> es
     , Time :> es
     )
  => Maybe Text
  -> Maybe Word
  -> Maybe Word
  -> (FloraM es) (Vector PackageName)
getPackagesByPrefixHandler maybePackageName maybeOffset maybeLimit =
  case maybePackageName of
    Nothing -> pure Vector.empty
    Just packageName -> do
      FloraEnv{pool} <- Reader.ask
      let offset = fromMaybe 0 maybeOffset
      let limit = fromMaybe 10 maybeLimit
      (_, packagesInfo) <- withReadOnlyPool pool $ searchPackageByName (offset, limit) packageName
      pure
        (Vector.map (\p -> p.name) packagesInfo)

getVersionedPackageHandler
  :: ( Error ServerError :> es
     , IOE :> es
     , IOE :> es
     , Reader FloraEnv :> es
     , Trace :> es
     )
  => Namespace
  -> PackageName
  -> Version
  -> (FloraM es) (PackageDTO 0)
getVersionedPackageHandler namespace packageName version = do
  FloraEnv{pool} <- Reader.ask
  package <- withReadOnlyPool pool $ guardThatPackageExists namespace packageName packageNotFound
  release <-
    withReadOnlyPool pool $
      guardThatReleaseExists package.packageId version $
        versionNotFound
          package.namespace
          package.name
  components <- withReadOnlyPool pool $ Query.getComponentsByReleaseId release.releaseId
  pure $ toPackageDTO package release components
