module FloraWeb.Pages.Server.Packages
  ( Routes
  , server
  )
where

import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable
import Data.Function
import Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Positive
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Vector qualified as Vector
import Distribution.Orphans ()
import Distribution.Types.Version (Version)
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (ask)
import Log (object, (.=))
import Log qualified
import Lucid
import Lucid.Orphans ()
import Servant (ServerT)
import Servant.Server (err404)

import Flora.Environment (FeatureEnv (..))
import Flora.Logging
import Flora.Model.BlobIndex.Query qualified as Query
import Flora.Model.Package
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Search qualified as Search
import FloraWeb.Common.Auth
import FloraWeb.Common.Guards
import FloraWeb.Common.Pagination
import FloraWeb.Pages.Routes.Packages
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Error
import FloraWeb.Pages.Templates.Packages qualified as Package
import FloraWeb.Pages.Templates.Pages.Packages qualified as Packages
import FloraWeb.Pages.Templates.Pages.Search qualified as Search
import FloraWeb.Session

server :: ServerT Routes FloraPage
server =
  Routes'
    { index = listPackagesHandler
    , showNamespace = showNamespaceHandler
    , showPackage = showPackageHandler
    , showVersion = showVersionHandler
    , showDependents = showDependentsHandler
    , showVersionDependents = showVersionDependentsHandler
    , showDependencies = showDependenciesHandler
    , showVersionDependencies = showVersionDependenciesHandler
    , showChangelog = showChangelogHandler
    , showVersionChangelog = showVersionChangelogHandler
    , listVersions = listVersionsHandler
    , getTarball = getTarballHandler
    }

listPackagesHandler :: Maybe (Positive Word) -> FloraPage (Html ())
listPackagesHandler pageParam = do
  let pageNumber = pageParam ?: PositiveUnsafe 1
  session <- getSession
  templateDefaults <- fromSession session defaultTemplateEnv
  (count', results) <- Search.listAllPackages (fromPage pageNumber)
  render templateDefaults $ Search.showAllPackages count' pageNumber results

showNamespaceHandler :: Namespace -> Maybe (Positive Word) -> FloraPage (Html ())
showNamespaceHandler namespace pageParam = do
  let pageNumber = pageParam ?: PositiveUnsafe 1
  session <- getSession
  templateDefaults <- fromSession session defaultTemplateEnv
  (count', results) <- Search.listAllPackagesInNamespace namespace (fromPage pageNumber)
  render templateDefaults $
    Search.showAllPackagesInNamespace namespace count' pageNumber results

showPackageHandler :: Namespace -> PackageName -> FloraPage (Html ())
showPackageHandler namespace packageName = showPackageVersion namespace packageName Nothing

showVersionHandler :: Namespace -> PackageName -> Version -> FloraPage (Html ())
showVersionHandler namespace packageName version =
  showPackageVersion namespace packageName (Just version)

showPackageVersion :: Namespace -> PackageName -> Maybe Version -> FloraPage (Html ())
showPackageVersion namespace packageName mversion = do
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  releases <- Query.getReleases package.packageId
  let latestRelease =
        releases
          & Vector.filter (\r -> Just True /= r.deprecated)
          & maximumBy (compare `on` (.version))
      version = fromMaybe latestRelease.version mversion
  release <- guardThatReleaseExists package.packageId version $ const web404
  numberOfReleases <- Query.getNumberOfReleases package.packageId
  dependents <- Query.getPackageDependents namespace packageName
  releaseDependencies <- Query.getRequirements release.releaseId
  categories <- Query.getPackageCategories package.packageId
  numberOfDependents <- Query.getNumberOfPackageDependents namespace packageName Nothing
  numberOfDependencies <- Query.getNumberOfPackageRequirements release.releaseId

  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = release.synopsis
          , indexPage = isNothing mversion
          }

  Log.logInfo "displaying a package" $
    object
      [ "release"
          .= object
            [ "id" .= release.releaseId
            , "version" .= display release.version
            ]
      , "dependencies"
          .= object
            [ "count" .= numberOfDependencies
            ]
      , "dependents"
          .= object
            [ "count" .= numberOfDependents
            ]
      , "package" .= (display namespace <> "/" <> display packageName)
      ]

  render templateEnv $
    Packages.showPackage
      release
      releases
      numberOfReleases
      package
      dependents
      numberOfDependents
      releaseDependencies
      numberOfDependencies
      categories

showDependentsHandler
  :: Namespace
  -> PackageName
  -> Maybe (Positive Word)
  -> Maybe Text
  -> FloraPage (Html ())
showDependentsHandler namespace packageName mPage mSearch = do
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  releases <- Query.getAllReleases package.packageId
  let latestRelease = maximumBy (compare `on` (.version)) releases
  showVersionDependentsHandler namespace packageName latestRelease.version mPage mSearch

showVersionDependentsHandler
  :: Namespace
  -> PackageName
  -> Version
  -> Maybe (Positive Word)
  -> Maybe Text
  -> FloraPage (Html ())
showVersionDependentsHandler namespace packageName version Nothing mSearch =
  showVersionDependentsHandler namespace packageName version (Just $ PositiveUnsafe 1) mSearch
showVersionDependentsHandler namespace packageName version pageNumber (Just "") =
  showVersionDependentsHandler namespace packageName version pageNumber Nothing
showVersionDependentsHandler namespace packageName version (Just pageNumber) mSearch = do
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  release <- guardThatReleaseExists package.packageId version (const web404)
  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = "Dependents of " <> display namespace <> display packageName
          }
  results <-
    Query.getAllPackageDependentsWithLatestVersion
      namespace
      packageName
      (fromPage pageNumber)
      mSearch

  totalDependents <- Query.getNumberOfPackageDependents namespace packageName mSearch
  render templateEnv $
    Package.showDependents
      namespace
      packageName
      release
      totalDependents
      results
      pageNumber
      mSearch

showDependenciesHandler :: Namespace -> PackageName -> FloraPage (Html ())
showDependenciesHandler namespace packageName = do
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  releases <- Query.getAllReleases package.packageId
  let latestRelease = maximumBy (compare `on` (.version)) releases
  showVersionDependenciesHandler namespace packageName latestRelease.version

showVersionDependenciesHandler :: Namespace -> PackageName -> Version -> FloraPage (Html ())
showVersionDependenciesHandler namespace packageName version = do
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  release <- guardThatReleaseExists package.packageId version $ const web404
  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = "Dependencies of " <> display namespace <> display packageName
          }
  (releaseDependencies, duration) <-
    timeAction $
      Query.getAllRequirements release.releaseId

  Log.logInfo "Retrieving all dependencies of the latest release of a package" $
    object
      [ "duration" .= duration
      , "package" .= (display namespace <> "/" <> display packageName)
      , "release_id" .= release.releaseId
      , "component_count" .= Map.size releaseDependencies
      , "dependencies_count" .= Map.foldl' (\acc ds -> acc + Vector.length ds) 0 releaseDependencies
      ]

  render templateEnv $
    Package.showDependencies namespace packageName release releaseDependencies

showChangelogHandler :: Namespace -> PackageName -> FloraPage (Html ())
showChangelogHandler namespace packageName = do
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  releases <- Query.getAllReleases package.packageId
  let latestRelease = maximumBy (compare `on` (.version)) releases
  showVersionChangelogHandler namespace packageName latestRelease.version

showVersionChangelogHandler :: Namespace -> PackageName -> Version -> FloraPage (Html ())
showVersionChangelogHandler namespace packageName version = do
  Log.logInfo_ $ display namespace
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  release <- guardThatReleaseExists package.packageId version $ const web404
  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = "Changelog of @" <> display namespace <> display packageName
          }

  render templateEnv $ Package.showChangelog namespace packageName version release.changelog

listVersionsHandler :: Namespace -> PackageName -> FloraPage (Html ())
listVersionsHandler namespace packageName = do
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = "Releases of " <> display namespace <> display packageName
          }
  releases <- Query.getAllReleases package.packageId
  render templateEnv $ Package.listVersions namespace packageName releases

constructTarballPath :: PackageName -> Version -> Text
constructTarballPath pname v = display pname <> "-" <> display v <> ".tar.gz"

getTarballHandler :: Namespace -> PackageName -> Version -> Text -> FloraPage ByteString
getTarballHandler namespace packageName version tarballName = do
  features <- ask @FeatureEnv
  unless (isJust $ features.blobStoreImpl) $! throwError err404
  package <- guardThatPackageExists namespace packageName $ \_ _ -> web404
  release <- guardThatReleaseExists package.packageId version $ const web404
  case release.tarballRootHash of
    Just rootHash
      | constructTarballPath packageName version == tarballName ->
          Query.queryTar packageName version rootHash
    _ -> throwError err404
