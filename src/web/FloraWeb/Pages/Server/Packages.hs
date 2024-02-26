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
import Servant (Headers (..), ServerT)
import Servant.Server (err404)

import Flora.Environment (FeatureEnv (..))
import Flora.Logging
import Flora.Model.BlobIndex.Query qualified as Query
import Flora.Model.Package
import Flora.Model.Package.Query qualified as Query
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types (PackageIndex (..))
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Model.User (User)
import Flora.Search qualified as Search
import FloraWeb.Common.Auth
import FloraWeb.Common.Guards
import FloraWeb.Common.Pagination
import FloraWeb.Pages.Routes.Packages
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Error
import FloraWeb.Pages.Templates.Packages qualified as Package
import FloraWeb.Pages.Templates.Screens.Packages qualified as Packages
import FloraWeb.Pages.Templates.Screens.Search qualified as Search
import FloraWeb.Types (FloraEff)
import Network.HTTP.Types (notFound404)

server :: ServerT Routes FloraEff
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

listPackagesHandler :: SessionWithCookies (Maybe User) -> Maybe (Positive Word) -> FloraEff (Html ())
listPackagesHandler (Headers session _) pageParam = do
  let pageNumber = pageParam ?: PositiveUnsafe 1
  templateDefaults <- templateFromSession session defaultTemplateEnv
  (count', results) <- Search.listAllPackages (fromPage pageNumber)
  render templateDefaults $ Search.showAllPackages count' pageNumber results

showNamespaceHandler :: SessionWithCookies (Maybe User) -> Namespace -> Maybe (Positive Word) -> FloraEff (Html ())
showNamespaceHandler (Headers session _) namespace pageParam = do
  let pageNumber = pageParam ?: PositiveUnsafe 1
  templateDefaults <- templateFromSession session defaultTemplateEnv
  (count', results) <- Search.listAllPackagesInNamespace (fromPage pageNumber) namespace
  if extractNamespaceText namespace == "haskell"
    then do
      let description = "Core Haskell packages"
      let templateEnv =
            templateDefaults
              { navbarSearchContent = Just $ "in:" <> display namespace <> " "
              , description = description
              }
      render templateEnv $
        Search.showAllPackagesInNamespace
          namespace
          description
          count'
          pageNumber
          results
    else do
      mPackageIndex <- Query.getPackageIndexByName (extractNamespaceText namespace)
      case mPackageIndex of
        Nothing -> renderError templateDefaults notFound404
        Just packageIndex -> do
          let templateEnv =
                templateDefaults
                  { navbarSearchContent = Just $ "in:" <> display namespace <> " "
                  , description = packageIndex.description
                  }
          render templateEnv $
            Search.showAllPackagesInNamespace namespace packageIndex.description count' pageNumber results

showPackageHandler :: SessionWithCookies (Maybe User) -> Namespace -> PackageName -> FloraEff (Html ())
showPackageHandler sessionWithCookies namespace packageName = showPackageVersion sessionWithCookies namespace packageName Nothing

showVersionHandler :: SessionWithCookies (Maybe User) -> Namespace -> PackageName -> Version -> FloraEff (Html ())
showVersionHandler sessionWithCookies namespace packageName version =
  showPackageVersion sessionWithCookies namespace packageName (Just version)

showPackageVersion :: SessionWithCookies (Maybe User) -> Namespace -> PackageName -> Maybe Version -> FloraEff (Html ())
showPackageVersion (Headers session _) namespace packageName mversion = do
  templateEnv' <- templateFromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404 session)
  packageIndex <- guardThatPackageIndexExists namespace $ const (web404 session)
  releases <- Query.getReleases package.packageId
  let latestRelease =
        releases
          & Vector.filter (\r -> r.deprecated /= Just True)
          & maximumBy (compare `on` (.version))
      version = fromMaybe latestRelease.version mversion
  release <- guardThatReleaseExists package.packageId version $ const (web404 session)
  numberOfReleases <- Query.getNumberOfReleases package.packageId
  dependents <- Query.getPackageDependents namespace packageName
  releaseDependencies <- Query.getRequirements package.name release.releaseId
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
      , "releases" .= numberOfReleases
      ]

  let packageIndexURL = packageIndex.url

  render templateEnv $
    Packages.showPackage
      release
      releases
      numberOfReleases
      package
      packageIndexURL
      dependents
      numberOfDependents
      releaseDependencies
      numberOfDependencies
      categories

showDependentsHandler
  :: SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Maybe (Positive Word)
  -> Maybe Text
  -> FloraEff (Html ())
showDependentsHandler s@(Headers session _) namespace packageName mPage mSearch = do
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404 session)
  releases <- Query.getAllReleases package.packageId
  let latestRelease = maximumBy (compare `on` (.version)) releases
  showVersionDependentsHandler s namespace packageName latestRelease.version mPage mSearch

showVersionDependentsHandler
  :: SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Version
  -> Maybe (Positive Word)
  -> Maybe Text
  -> FloraEff (Html ())
showVersionDependentsHandler s namespace packageName version Nothing mSearch =
  showVersionDependentsHandler s namespace packageName version (Just $ PositiveUnsafe 1) mSearch
showVersionDependentsHandler s namespace packageName version pageNumber (Just "") =
  showVersionDependentsHandler s namespace packageName version pageNumber Nothing
showVersionDependentsHandler (Headers session _) namespace packageName version (Just pageNumber) mSearch = do
  templateEnv' <- templateFromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404 session)
  release <- guardThatReleaseExists package.packageId version (const (web404 session))
  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = "Dependents of " <> display namespace <> "/" <> display packageName
          , navbarSearchContent = Just $ "depends:" <> display namespace <> "/" <> display packageName <> " "
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

showDependenciesHandler :: SessionWithCookies (Maybe User) -> Namespace -> PackageName -> FloraEff (Html ())
showDependenciesHandler s@(Headers session _) namespace packageName = do
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404 session)
  releases <- Query.getAllReleases package.packageId
  let latestRelease = maximumBy (compare `on` (.version)) releases
  showVersionDependenciesHandler s namespace packageName latestRelease.version

showVersionDependenciesHandler :: SessionWithCookies (Maybe User) -> Namespace -> PackageName -> Version -> FloraEff (Html ())
showVersionDependenciesHandler (Headers session _) namespace packageName version = do
  templateEnv' <- templateFromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404 session)
  release <- guardThatReleaseExists package.packageId version $ const (web404 session)
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

showChangelogHandler :: SessionWithCookies (Maybe User) -> Namespace -> PackageName -> FloraEff (Html ())
showChangelogHandler s@(Headers session _) namespace packageName = do
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404 session)
  releases <- Query.getAllReleases package.packageId
  let latestRelease = maximumBy (compare `on` (.version)) releases
  showVersionChangelogHandler s namespace packageName latestRelease.version

showVersionChangelogHandler :: SessionWithCookies (Maybe User) -> Namespace -> PackageName -> Version -> FloraEff (Html ())
showVersionChangelogHandler (Headers session _) namespace packageName version = do
  Log.logInfo_ $ display namespace
  templateEnv' <- templateFromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404 session)
  release <- guardThatReleaseExists package.packageId version $ const (web404 session)
  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = "Changelog of @" <> display namespace <> display packageName
          }

  render templateEnv $ Package.showChangelog namespace packageName version release.changelog

listVersionsHandler :: SessionWithCookies (Maybe User) -> Namespace -> PackageName -> FloraEff (Html ())
listVersionsHandler (Headers session _) namespace packageName = do
  templateEnv' <- templateFromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404 session)
  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = "Releases of " <> display namespace <> display packageName
          }
  releases <- Query.getAllReleases package.packageId
  render templateEnv $ Package.listVersions namespace packageName releases

constructTarballPath :: PackageName -> Version -> Text
constructTarballPath pname v = display pname <> "-" <> display v <> ".tar.gz"

getTarballHandler :: SessionWithCookies (Maybe User) -> Namespace -> PackageName -> Version -> Text -> FloraEff ByteString
getTarballHandler (Headers session _) namespace packageName version tarballName = do
  features <- ask @FeatureEnv
  unless (isJust features.blobStoreImpl) $ throwError err404
  package <- guardThatPackageExists namespace packageName $ \_ _ -> web404 session
  release <- guardThatReleaseExists package.packageId version $ const (web404 session)
  case release.tarballRootHash of
    Just rootHash
      | constructTarballPath packageName version == tarballName ->
          Query.queryTar packageName version rootHash
    _ -> throwError err404
