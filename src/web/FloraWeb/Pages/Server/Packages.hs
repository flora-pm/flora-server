module FloraWeb.Pages.Server.Packages
  ( Routes
  , server
  )
where

import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable
import Data.Function
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Vector qualified as Vector
import Data.Vector.Algorithms.Intro qualified as MVector
import Distribution.Types.Version (Version)
import Effectful (IOE, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (Reader, ask)
import Effectful.Time (Time)
import Effectful.Trace
import Log (object, (.=))
import Log qualified
import Lucid
import Monitor.Tracing qualified as Tracing
import Network.HTTP.Types (notFound404)
import RequireCallStack
import Servant (Headers (..), ServerError, ServerT)
import Servant.Server (err404)

import Advisories.Model.Affected.Query qualified as Query
import Advisories.Model.Affected.Types
import Data.Positive
import Distribution.Orphans ()
import Flora.Environment.Env (FeatureEnv (..))
import Flora.Model.BlobIndex.Query qualified as Query
import Flora.Model.BlobStore.API (BlobStoreAPI)
import Flora.Model.Package
import Flora.Model.Package.Guard
import Flora.Model.Package.Query qualified as Query
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types (PackageIndex (..))
import Flora.Model.Release.Guard
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Model.User (User)
import Flora.Monad
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
import Lucid.Orphans ()

server :: RequireCallStack => ServerT Routes FloraEff
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
    , showPackageSecurity = showPackageSecurityHandler
    }

listPackagesHandler
  :: ( DB :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Trace :> es
     )
  => SessionWithCookies (Maybe User)
  -> Maybe (Positive Word)
  -> FloraM es (Html ())
listPackagesHandler (Headers session _) pageParam = do
  Tracing.rootSpan alwaysSampled "list-all-packages" $ do
    let pageNumber = pageParam ?: PositiveUnsafe 1
    templateDefaults <- templateFromSession session defaultTemplateEnv
    (count', results) <- Search.listAllPackages (fromPage pageNumber)
    render templateDefaults $ Search.showAllPackages count' pageNumber results

showNamespaceHandler
  :: ( DB :> es
     , Error ServerError :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Time :> es
     , Trace :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> Maybe (Positive Word)
  -> FloraM es (Html ())
showNamespaceHandler (Headers session _) packageNamespace pageParam =
  Tracing.rootSpan alwaysSampled "show-namespace" $ do
    let pageNumber = pageParam ?: PositiveUnsafe 1
    templateDefaults <- templateFromSession session defaultTemplateEnv
    (count', results) <- Search.listAllPackagesInNamespace (fromPage pageNumber) packageNamespace
    if extractNamespaceText packageNamespace == "haskell"
      then do
        let description = "Core Haskell packages"
        let templateEnv =
              templateDefaults
                { navbarSearchContent = Just $ "in:" <> display packageNamespace <> " "
                , description = description
                }
        render templateEnv $
          Search.showAllPackagesInNamespace
            packageNamespace
            description
            count'
            pageNumber
            results
      else do
        mPackageIndex <- Query.getPackageIndexByName (extractNamespaceText packageNamespace)
        case mPackageIndex of
          Nothing -> renderError templateDefaults notFound404
          Just packageIndex -> do
            let templateEnv =
                  templateDefaults
                    { navbarSearchContent = Just $ "in:" <> display packageNamespace <> " "
                    , description = packageIndex.description
                    }
            render templateEnv $
              Search.showAllPackagesInNamespace packageNamespace packageIndex.description count' pageNumber results

showPackageHandler
  :: ( DB :> es
     , Error ServerError :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Trace :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> FloraM es (Html ())
showPackageHandler sessionWithCookies packageNamespace packageName =
  showPackageVersion sessionWithCookies packageNamespace packageName Nothing

showVersionHandler
  :: ( DB :> es
     , Error ServerError :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Trace :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Version
  -> FloraM es (Html ())
showVersionHandler sessionWithCookies packageNamespace packageName version =
  showPackageVersion sessionWithCookies packageNamespace packageName (Just version)

showPackageVersion
  :: ( DB :> es
     , Error ServerError :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Trace :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Maybe Version
  -> FloraM es (Html ())
showPackageVersion (Headers session _) packageNamespace packageName mversion =
  Tracing.rootSpan alwaysSampled "show-package-with-version" $ do
    templateEnv' <- templateFromSession session defaultTemplateEnv
    package <- guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
    packageIndex <- guardThatPackageIndexExists packageNamespace $ const (web404 session)
    releases <-
      Tracing.childSpan "Query.getReleases" $
        Query.getReleases package.packageId
    let latestRelease =
          releases
            & Vector.filter (\r -> r.deprecated /= Just True)
            & maximumBy (compare `on` (.version))
        version = fromMaybe latestRelease.version mversion
    release <- guardThatReleaseExists package.packageId version $ const (web404 session)
    numberOfReleases <- Query.getNumberOfReleases package.packageId
    dependents <-
      Tracing.childSpan "Query.getPackageDependents" $
        Query.getPackageDependents packageNamespace packageName
    releaseDependencies <-
      Tracing.childSpan "Query.getRequirements" $
        Query.getRequirements package.name release.releaseId
    categories <- Query.getPackageCategories package.packageId
    numberOfDependents <-
      Tracing.childSpan "Query.getNumberOfPackageDependents" $
        Query.getNumberOfPackageDependents packageNamespace packageName Nothing
    numberOfDependencies <- Query.getNumberOfPackageRequirements release.releaseId

    let templateEnv =
          templateEnv'
            { title = display packageNamespace <> "/" <> display packageName
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
        , "package" .= (display packageNamespace <> "/" <> display packageName)
        , "releases" .= numberOfReleases
        ]

    let packageIndexURL = packageIndex.url

    Tracing.childSpan "render showPackage" $
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
  :: ( DB :> es
     , Error ServerError :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Time :> es
     , Trace :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Maybe (Positive Word)
  -> Maybe Text
  -> FloraM es (Html ())
showDependentsHandler s@(Headers session _) packageNamespace packageName mPage mSearch = do
  package <- guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
  maybeLatestRelease <- Query.getLatestPackageRelease package.packageId

  case maybeLatestRelease of
    Nothing -> throwError err404
    Just latestRelease ->
      showVersionDependentsHandler s packageNamespace packageName latestRelease.version mPage mSearch

showVersionDependentsHandler
  :: ( DB :> es
     , Error ServerError :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Time :> es
     , Trace :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Version
  -> Maybe (Positive Word)
  -> Maybe Text
  -> FloraM es (Html ())
showVersionDependentsHandler s packageNamespace packageName version Nothing mSearch =
  showVersionDependentsHandler s packageNamespace packageName version (Just $ PositiveUnsafe 1) mSearch
showVersionDependentsHandler s packageNamespace packageName version pageNumber (Just "") =
  showVersionDependentsHandler s packageNamespace packageName version pageNumber Nothing
showVersionDependentsHandler (Headers session _) packageNamespace packageName version (Just pageNumber) mSearch = do
  Tracing.rootSpan alwaysSampled "show-package-version-dependents" $ do
    templateEnv' <- templateFromSession session defaultTemplateEnv
    package <- guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
    release <- guardThatReleaseExists package.packageId version (const (web404 session))
    let templateEnv =
          templateEnv'
            { title = display packageNamespace <> "/" <> display packageName
            , description = "Dependents of " <> display packageNamespace <> "/" <> display packageName
            , navbarSearchContent = Just $ "depends:" <> display packageNamespace <> "/" <> display packageName <> " "
            }
    results <-
      Tracing.childSpan "Query.getPackageDependents" $
        Query.getAllPackageDependentsWithLatestVersion
          packageNamespace
          packageName
          (fromPage pageNumber)
          mSearch

    totalDependents <- Query.getNumberOfPackageDependents packageNamespace packageName mSearch
    Tracing.childSpan "render showDependents" $
      render templateEnv $
        Package.showDependents
          packageNamespace
          packageName
          release
          totalDependents
          results
          pageNumber

showDependenciesHandler
  :: ( DB :> es
     , Error ServerError :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Trace :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> FloraM es (Html ())
showDependenciesHandler s@(Headers session _) packageNamespace packageName = do
  package <- guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
  maybeLatestRelease <- Query.getLatestPackageRelease package.packageId

  case maybeLatestRelease of
    Nothing -> throwError err404
    Just latestRelease ->
      showVersionDependenciesHandler s packageNamespace packageName latestRelease.version

showVersionDependenciesHandler
  :: ( DB :> es
     , Error ServerError :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Trace :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Version
  -> FloraM es (Html ())
showVersionDependenciesHandler (Headers session _) packageNamespace packageName version = do
  Tracing.rootSpan alwaysSampled "show-version-dependencies" $ do
    templateEnv' <- templateFromSession session defaultTemplateEnv
    package <- guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
    release <- guardThatReleaseExists package.packageId version $ const (web404 session)
    let templateEnv =
          templateEnv'
            { title = display packageNamespace <> "/" <> display packageName
            , description = "Dependencies of " <> display packageNamespace <> display packageName
            }
    releaseDependencies <-
      Tracing.childSpan "Query.getAllRequirements" $
        Query.getAllRequirements release.releaseId

    Tracing.childSpan "render showDependencies" $
      render templateEnv $
        Package.showDependencies packageNamespace packageName release releaseDependencies

showChangelogHandler
  :: ( DB :> es
     , Error ServerError :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Trace :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> FloraM es (Html ())
showChangelogHandler s@(Headers session _) packageNamespace packageName = do
  Tracing.rootSpan alwaysSampled "show-changelog" $ do
    package <- guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
    maybeLatestRelease <-
      Tracing.childSpan "Query.getLatestPackageRelease" $
        Query.getLatestPackageRelease package.packageId

    case maybeLatestRelease of
      Nothing -> throwError err404
      Just latestRelease ->
        showVersionChangelogHandler s packageNamespace packageName latestRelease.version

showVersionChangelogHandler
  :: ( DB :> es
     , Error ServerError :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Trace :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Version
  -> FloraM es (Html ())
showVersionChangelogHandler (Headers session _) packageNamespace packageName version = do
  Tracing.rootSpan alwaysSampled "show-version-changelog" $ do
    templateEnv' <- templateFromSession session defaultTemplateEnv
    package <- guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
    release <- guardThatReleaseExists package.packageId version $ const (web404 session)
    let templateEnv =
          templateEnv'
            { title = display packageNamespace <> "/" <> display packageName
            , description = "Changelog of " <> display packageNamespace <> "/" <> display packageName
            }

    render templateEnv $ Package.showChangelog packageNamespace packageName version release.changelog

listVersionsHandler
  :: ( DB :> es
     , Error ServerError :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Trace :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> FloraM es (Html ())
listVersionsHandler (Headers session _) packageNamespace packageName = do
  templateEnv' <- templateFromSession session defaultTemplateEnv
  package <- guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
  let templateEnv =
        templateEnv'
          { title = display packageNamespace <> "/" <> display packageName
          , description = "Releases of " <> display packageNamespace <> display packageName
          }
  releases <- Query.getAllReleases package.packageId
  render templateEnv $ Package.listVersions packageNamespace packageName releases

constructTarballPath :: PackageName -> Version -> Text
constructTarballPath pname v = display pname <> "-" <> display v <> ".tar.gz"

getTarballHandler
  :: ( BlobStoreAPI :> es
     , DB :> es
     , Error ServerError :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Trace :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Version
  -> Text
  -> FloraM es ByteString
getTarballHandler (Headers session _) packageNamespace packageName version tarballName = do
  features <- ask @FeatureEnv
  unless (isJust features.blobStoreImpl) $ throwError err404
  package <- guardThatPackageExists packageNamespace packageName $ \_ _ -> web404 session
  release <- guardThatReleaseExists package.packageId version $ const (web404 session)
  case release.tarballRootHash of
    Just rootHash
      | constructTarballPath packageName version == tarballName ->
          Query.queryTar packageName version rootHash
    _ -> throwError err404

showPackageSecurityHandler
  :: ( DB :> es
     , Error ServerError :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Trace :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> FloraM es (Html ())
showPackageSecurityHandler (Headers session _) packageNamespace packageName =
  Tracing.rootSpan alwaysSampled "show-package-security" $ do
    templateEnv' <- templateFromSession session defaultTemplateEnv
    package <- guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
    advisoryPreviews <-
      Tracing.childSpan "Query.getAdvisoryPreviewsByPackageId" $
        Query.getAdvisoryPreviewsByPackageId package.packageId
    let templateEnv =
          templateEnv'
            { title = display packageNamespace <> "/" <> display packageName
            , description = "Releases of " <> display packageNamespace <> display packageName
            }
    render templateEnv $
      Package.showPackageSecurityPage
        packageNamespace
        packageName
        (Vector.reverse $ Vector.modify (MVector.sortBy (\v1 v2 -> compare v1.hsecId v2.hsecId)) advisoryPreviews)
