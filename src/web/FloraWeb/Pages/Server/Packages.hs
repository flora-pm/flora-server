module FloraWeb.Pages.Server.Packages
  ( Routes
  , server
  )
where

import Control.Monad
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
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Effectful.Tracing (Tracer)
import Effectful.Tracing qualified as Trace
import Log (object, (.=))
import Log qualified
import Lucid
import Network.HTTP.Types (notFound404)
import RequireCallStack
import Servant (Headers (..), ServerError, ServerT)
import Servant.Server (err404)

import Advisories.Model.Affected.Query qualified as Query
import Advisories.Model.Affected.Types
import Data.Positive
import Distribution.Orphans ()
import Flora.Database
import Flora.Environment.Env (FeatureEnv (..), FloraEnv (..))
import Flora.Model.BlobIndex.Query qualified as Query
import Flora.Model.BlobStore.API (BlobStoreAPI)
import Flora.Model.Package.Guard
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Model.PackageGroupPackage.Query qualified as Query
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types (PackageIndex (..))
import Flora.Model.PackageMaintainer.Query qualified as Query
import Flora.Model.PackageUploader.Query qualified as Query
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
  :: ( IOE :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , Time.Time :> es
     , Tracer :> es
     )
  => SessionWithCookies (Maybe User)
  -> Maybe (Positive Word)
  -> FloraM es (Html ())
listPackagesHandler (Headers session _) pageParam = do
  Trace.withLinkedRoot [] $ Trace.withSpan "list-all-packages" $ do
    let pageNumber = pageParam ?: PositiveUnsafe 1
    templateEnv' <- templateFromSession session defaultTemplateEnv
    now <- Time.currentTime
    (count', results) <- Search.listAllPackages (fromPage pageNumber)
    let templateEnv =
          templateEnv'
            { title = "Packages — Flora.pm"
            , description = "List of packages"
            }
    render templateEnv $ Search.showAllPackages now count' pageNumber results

showNamespaceHandler
  :: ( Error ServerError :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , Time.Time :> es
     , Tracer :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> Maybe (Positive Word)
  -> FloraM es (Html ())
showNamespaceHandler (Headers session _) packageNamespace pageParam =
  Trace.withLinkedRoot [] $ Trace.withSpan "show-namespace" $ do
    FloraEnv{pool} <- Reader.ask
    let pageNumber = pageParam ?: PositiveUnsafe 1
    templateDefaults <- templateFromSession session defaultTemplateEnv
    (count', results) <- Search.listAllPackagesInNamespace (fromPage pageNumber) packageNamespace
    mPackageIndex <- withReadOnlyPool pool $ Query.getPackageIndexByName (extractNamespaceText packageNamespace)
    now <- Time.currentTime
    case mPackageIndex of
      Nothing -> renderError templateDefaults notFound404
      Just packageIndex -> do
        let templateEnv =
              templateDefaults
                { navbarSearchContent = Just $ "in:" <> display packageNamespace <> " "
                , title = "Packages in " <> display packageNamespace <> " — Flora.pm"
                , description = packageIndex.description
                }
        render templateEnv $
          Search.showAllPackagesInNamespace now packageNamespace packageIndex.description count' pageNumber results

showPackageHandler
  :: ( Error ServerError :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , Tracer :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> FloraM es (Html ())
showPackageHandler sessionWithCookies packageNamespace packageName =
  showPackageVersion sessionWithCookies packageNamespace packageName Nothing

showVersionHandler
  :: ( Error ServerError :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , Tracer :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Version
  -> FloraM es (Html ())
showVersionHandler sessionWithCookies packageNamespace packageName version =
  showPackageVersion sessionWithCookies packageNamespace packageName (Just version)

showPackageVersion
  :: ( Error ServerError :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , Tracer :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Maybe Version
  -> FloraM es (Html ())
showPackageVersion (Headers session _) packageNamespace packageName mversion =
  Trace.withLinkedRoot [] $ Trace.withSpan "show-package-with-version" $ do
    FloraEnv{pool} <- Reader.ask
    templateEnv' <- templateFromSession session defaultTemplateEnv
    package <- withReadOnlyPool pool $ guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
    packageIndex <- guardThatPackageIndexExists packageNamespace $ const (web404 session)
    releases <-
      Trace.withSpan "Query.getReleases" $
        withReadOnlyPool pool $
          Query.getReleases package.packageId
    let latestRelease =
          releases
            & Vector.filter (\r -> r.deprecated /= Just True)
            & maximumBy (compare `on` (.version))
        version = fromMaybe latestRelease.version mversion
    release <- withReadOnlyPool pool $ guardThatReleaseExists package.packageId version $ const (web404 session)
    numberOfReleases <- withReadOnlyPool pool $ Query.getNumberOfReleases package.packageId
    dependents <-
      Trace.withSpan "Query.getPackageDependents" $
        withReadOnlyPool pool $
          Query.getPackageDependents packageNamespace packageName
    releaseDependencies <-
      Trace.withSpan "Query.getRequirements" $
        withReadOnlyPool pool $
          Query.getRequirements package.name release.releaseId
    categories <- withReadOnlyPool pool $ Query.getPackageCategories package.packageId
    numberOfDependents <-
      Trace.withSpan "Query.getNumberOfPackageDependents" $
        withReadOnlyPool pool $
          Query.getNumberOfPackageDependents packageNamespace packageName Nothing
    numberOfDependencies <- withReadOnlyPool pool $ Query.getNumberOfPackageRequirements release.releaseId
    groups <- withReadOnlyPool pool $ Query.getPackageGroupsForPackage package.packageId
    activeMaintainers <-
      if package.namespace == Namespace "hackage"
        then withReadOnlyPool pool $ Just <$> Query.getActiveMaintainers package.packageId
        else pure Nothing
    mUploader <- join <$> (traverse (\u -> withReadOnlyPool pool $ Query.getPackageUploaderById u) release.uploaderId)

    let templateEnv =
          templateEnv'
            { title = display packageNamespace <> " › " <> display packageName <> " — Flora.pm"
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

    Trace.withSpan "render showPackage" $
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
          groups
          activeMaintainers
          mUploader

showDependentsHandler
  :: ( Error ServerError :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , Time.Time :> es
     , Tracer :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Maybe (Positive Word)
  -> Maybe Text
  -> FloraM es (Html ())
showDependentsHandler s@(Headers session _) packageNamespace packageName mPage mSearch = do
  FloraEnv{pool} <- Reader.ask
  package <- withReadOnlyPool pool $ guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
  maybeLatestRelease <- withReadOnlyPool pool $ Query.getLatestPackageRelease package.packageId
  case maybeLatestRelease of
    Nothing -> throwError err404
    Just latestRelease ->
      showVersionDependentsHandler s packageNamespace packageName latestRelease.version mPage mSearch

showVersionDependentsHandler
  :: ( Error ServerError :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , Time.Time :> es
     , Tracer :> es
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
  Trace.withLinkedRoot [] $ Trace.withSpan "show-package-version-dependents" $ do
    FloraEnv{pool} <- Reader.ask
    templateEnv' <- templateFromSession session defaultTemplateEnv
    package <- withReadOnlyPool pool $ guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
    release <- withReadOnlyPool pool $ guardThatReleaseExists package.packageId version (const (web404 session))
    let templateEnv =
          templateEnv'
            { title = display packageNamespace <> "/" <> display packageName
            , description = "Dependents of " <> display packageNamespace <> "/" <> display packageName
            , navbarSearchContent = Just $ "depends:" <> display packageNamespace <> "/" <> display packageName <> " "
            }
    results <-
      Trace.withSpan "Query.getPackageDependents" $
        withReadOnlyPool pool $
          Query.getAllPackageDependentsWithLatestVersion
            packageNamespace
            packageName
            (fromPage pageNumber)
            mSearch

    numberOfDependents <- withReadOnlyPool pool $ Query.getNumberOfPackageDependents packageNamespace packageName mSearch
    numberOfDependencies <- withReadOnlyPool pool $ Query.getNumberOfPackageRequirements release.releaseId
    numberOfReleases <- withReadOnlyPool pool $ Query.getNumberOfReleases package.packageId
    now <- Time.currentTime

    Trace.withSpan "render showDependents" $
      render templateEnv $
        Package.showDependents
          now
          numberOfReleases
          release
          numberOfDependencies
          numberOfDependents
          packageNamespace
          packageName
          results
          pageNumber

showDependenciesHandler
  :: ( Error ServerError :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , Time.Time :> es
     , Tracer :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> FloraM es (Html ())
showDependenciesHandler s@(Headers session _) packageNamespace packageName = do
  FloraEnv{pool} <- Reader.ask
  package <- withReadOnlyPool pool $ guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
  maybeLatestRelease <- withReadOnlyPool pool $ Query.getLatestPackageRelease package.packageId
  case maybeLatestRelease of
    Nothing -> throwError err404
    Just latestRelease ->
      showVersionDependenciesHandler s packageNamespace packageName latestRelease.version

showVersionDependenciesHandler
  :: ( Error ServerError :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , Time.Time :> es
     , Tracer :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Version
  -> FloraM es (Html ())
showVersionDependenciesHandler (Headers session _) packageNamespace packageName version = do
  Trace.withLinkedRoot [] $ Trace.withSpan "show-version-dependencies" $ do
    FloraEnv{pool} <- Reader.ask
    templateEnv' <- templateFromSession session defaultTemplateEnv
    package <- withReadOnlyPool pool $ guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
    release <- withReadOnlyPool pool $ guardThatReleaseExists package.packageId version $ const (web404 session)
    numberOfDependents <- withReadOnlyPool pool $ Query.getNumberOfPackageDependents packageNamespace packageName Nothing
    numberOfDependencies <- withReadOnlyPool pool $ Query.getNumberOfPackageRequirements release.releaseId
    numberOfReleases <- withReadOnlyPool pool $ Query.getNumberOfReleases package.packageId
    let templateEnv =
          templateEnv'
            { title = display packageNamespace <> " › " <> display packageName <> " › dependencies — Flora.pm"
            , description = "Dependencies of " <> display packageNamespace <> display packageName
            }
    releaseDependencies <-
      Trace.withSpan "Query.getAllRequirements" $
        withReadOnlyPool pool $
          Query.getAllRequirements release.releaseId

    now <- Time.currentTime
    Trace.withSpan "render showDependencies" $
      render templateEnv $
        Package.showDependencies
          now
          numberOfReleases
          release
          numberOfDependencies
          numberOfDependents
          packageNamespace
          packageName
          releaseDependencies

showChangelogHandler
  :: ( Error ServerError :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , Tracer :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> FloraM es (Html ())
showChangelogHandler s@(Headers session _) packageNamespace packageName = do
  Trace.withLinkedRoot [] $ Trace.withSpan "show-changelog" $ do
    FloraEnv{pool} <- Reader.ask
    package <- withReadOnlyPool pool $ guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
    maybeLatestRelease <-
      Trace.withSpan "Query.getLatestPackageRelease" $
        withReadOnlyPool pool $
          Query.getLatestPackageRelease package.packageId
    case maybeLatestRelease of
      Nothing -> throwError err404
      Just latestRelease ->
        showVersionChangelogHandler s packageNamespace packageName latestRelease.version

showVersionChangelogHandler
  :: ( Error ServerError :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , Tracer :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Version
  -> FloraM es (Html ())
showVersionChangelogHandler (Headers session _) packageNamespace packageName version = do
  Trace.withLinkedRoot [] $ Trace.withSpan "show-version-changelog" $ do
    FloraEnv{pool} <- Reader.ask
    templateEnv' <- templateFromSession session defaultTemplateEnv
    package <- withReadOnlyPool pool $ guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
    release <- withReadOnlyPool pool $ guardThatReleaseExists package.packageId version $ const (web404 session)
    numberOfDependents <- withReadOnlyPool pool $ Query.getNumberOfPackageDependents packageNamespace packageName Nothing
    numberOfDependencies <- withReadOnlyPool pool $ Query.getNumberOfPackageRequirements release.releaseId
    numberOfReleases <- withReadOnlyPool pool $ Query.getNumberOfReleases package.packageId
    let templateEnv =
          templateEnv'
            { title = display packageNamespace <> "/" <> display packageName
            , description = "Changelog of " <> display packageNamespace <> "/" <> display packageName
            }

    render templateEnv $
      Package.showChangelog
        numberOfReleases
        release
        numberOfDependencies
        numberOfDependents
        packageNamespace
        packageName
        release.changelog

listVersionsHandler
  :: ( Error ServerError :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , Time.Time :> es
     , Tracer :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> FloraM es (Html ())
listVersionsHandler (Headers session _) packageNamespace packageName = do
  FloraEnv{pool} <- Reader.ask
  templateEnv' <- templateFromSession session defaultTemplateEnv
  now <- Time.currentTime
  package <- withReadOnlyPool pool $ guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
  maybeLatestRelease <- withReadOnlyPool pool $ Query.getLatestPackageRelease package.packageId
  case maybeLatestRelease of
    Nothing -> throwError err404
    Just latestRelease -> do
      let templateEnv =
            templateEnv'
              { title = display packageNamespace <> "/" <> display packageName
              , description = "Releases of " <> display packageNamespace <> display packageName
              }
      numberOfDependents <-
        Trace.withSpan "Query.getNumberOfPackageDependents" $
          withReadOnlyPool pool $
            Query.getNumberOfPackageDependents packageNamespace packageName Nothing
      numberOfDependencies <- withReadOnlyPool pool $ Query.getNumberOfPackageRequirements latestRelease.releaseId

      releases <- withReadOnlyPool pool $ Query.getAllReleases package.packageId

      render templateEnv $
        Package.listVersions
          latestRelease
          now
          numberOfDependencies
          numberOfDependents
          packageNamespace
          packageName
          latestRelease.synopsis
          releases

constructTarballPath :: PackageName -> Version -> Text
constructTarballPath pname v = display pname <> "-" <> display v <> ".tar.gz"

getTarballHandler
  :: ( BlobStoreAPI :> es
     , Error ServerError :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , Tracer :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Version
  -> Text
  -> FloraM es ByteString
getTarballHandler (Headers session _) packageNamespace packageName version tarballName = do
  FloraEnv{pool} <- Reader.ask
  features <- Reader.ask @FeatureEnv
  unless (isJust features.blobStoreImpl) $ throwError err404
  package <- withReadOnlyPool pool $ guardThatPackageExists packageNamespace packageName $ \_ _ -> web404 session
  release <- withReadOnlyPool pool $ guardThatReleaseExists package.packageId version $ const (web404 session)
  case release.tarballRootHash of
    Just rootHash
      | constructTarballPath packageName version == tarballName ->
          withReadOnlyPool pool $
            Query.queryTar packageName version rootHash
    _ -> throwError err404

showPackageSecurityHandler
  :: ( Error ServerError :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , Tracer :> es
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> FloraM es (Html ())
showPackageSecurityHandler (Headers session _) packageNamespace packageName =
  Trace.withLinkedRoot [] $ Trace.withSpan "show-package-security" $ do
    FloraEnv{pool} <- Reader.ask
    templateEnv' <- templateFromSession session defaultTemplateEnv
    package <- withReadOnlyPool pool $ guardThatPackageExists packageNamespace packageName (\_ _ -> web404 session)
    maybeLatestRelease <- withReadOnlyPool pool $ Query.getLatestPackageRelease package.packageId
    case maybeLatestRelease of
      Nothing -> throwError err404
      Just latestRelease -> do
        advisoryPreviews <-
          Trace.withSpan "Query.getAdvisoryPreviewsByPackageId" $
            withReadOnlyPool pool $
              Query.getAdvisoryPreviewsByPackageId package.packageId
        let templateEnv =
              templateEnv'
                { title = display packageNamespace <> "/" <> display packageName
                , description = "Releases of " <> display packageNamespace <> display packageName
                }
        numberOfDependents <- withReadOnlyPool pool $ Query.getNumberOfPackageDependents packageNamespace packageName Nothing
        numberOfDependencies <- withReadOnlyPool pool $ Query.getNumberOfPackageRequirements latestRelease.releaseId
        numberOfReleases <- withReadOnlyPool pool $ Query.getNumberOfReleases package.packageId
        render templateEnv $
          Package.showPackageSecurityPage
            latestRelease
            numberOfDependencies
            numberOfDependents
            packageNamespace
            packageName
            latestRelease.synopsis
            numberOfReleases
            (Vector.reverse $ Vector.modify (MVector.sortBy (\v1 v2 -> compare v1.hsecId v2.hsecId)) advisoryPreviews)
