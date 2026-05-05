module FloraJobs.Runner where

import Arbiter.Core
import Arbiter.Core qualified as Arb
import Arbiter.Simple qualified as ArbS
import Control.Concurrent (forkIO)
import Control.Concurrent.Async qualified as Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Types.Version (Version)
import Effectful (IOE, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.Log hiding (LogLevel)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Process.Typed
import Effectful.Prometheus
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Shared (State)
import Effectful.Time (Time)
import Log hiding (LogLevel)
import Network.HTTP.Types (gone410, notFound404, statusCode)
import RequireCallStack
import Servant.Client (ClientError (..))
import Servant.Client.Core (ResponseF (..))
import System.FilePath

import Data.Text.HTML qualified as HTML
import Flora.Environment.Env
import Flora.Import.Package (persistImportOutput)
import Flora.Import.Package.Bulk.Archive qualified as Import
import Flora.Import.Types
import Flora.Model.BlobIndex.Update qualified as Update
import Flora.Model.Job
import Flora.Model.Package.Types
import Flora.Model.Package.Update qualified as Update
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Model.Release.Update qualified as Update
import Flora.Monad
import FloraJobs.Environment
import FloraJobs.Render (renderMarkdown)
import FloraJobs.Scheduler
import FloraJobs.ThirdParties.Hackage.API
  ( HackagePackageInfo (..)
  , HackagePreferredVersions (..)
  , VersionedPackage (..)
  )
import FloraJobs.ThirdParties.Hackage.Client qualified as Hackage
import FloraJobs.Types

runner :: RequireCallStack => ArbS.SimpleEnv JobQueues -> JobRead PackageJob -> JobsRunner ()
runner env job = case job.payload of
  FetchReadme x -> makeReadme x
  FetchTarball x -> fetchTarball x
  FetchUploadInformation x -> fetchUploadInformation x
  FetchChangelog x -> fetchChangeLog x
  ImportPackage x -> persistImportOutput x
  FetchPackageDeprecationList -> fetchPackageDeprecationList
  FetchReleaseDeprecationList packageName releases -> fetchReleaseDeprecationList packageName releases
  RefreshLatestVersions -> Update.refreshLatestVersions
  RefreshIndex indexName -> refreshIndex env indexName

fetchChangeLog :: RequireCallStack => ChangelogJobPayload -> JobsRunner ()
fetchChangeLog ChangelogJobPayload{packageName, packageVersion, releaseId} =
  localDomain "fetch-changelog" $ do
    let requestPayload = VersionedPackage packageName packageVersion
    result <- Hackage.request $ Hackage.getPackageChangelog requestPayload
    case result of
      Left e -> handleClientError e
      Right bodyText -> do
        changelogBody <- renderMarkdown ("CHANGELOG" <> show packageName) bodyText
        Update.updateChangelog releaseId (Just $ HTML.fromText changelogBody) Imported
  where
    handleClientError :: ClientError -> JobsRunner ()
    handleClientError e@(FailureResponse _ response)
      -- If the CHANGELOG simply doesn't exist, we skip it by marking the job as successful.
      | response.responseStatusCode == notFound404 = Update.updateChangelog releaseId Nothing Inexistent
      | response.responseStatusCode == gone410 = Update.updateChangelog releaseId Nothing Inexistent
      | otherwise = Arb.throwRetryable (Text.show e)
    handleClientError e = Arb.throwRetryable (Text.show e)

makeReadme :: RequireCallStack => ReadmeJobPayload -> JobsRunner ()
makeReadme ReadmeJobPayload{mpPackage, mpReleaseId, mpVersion} =
  localDomain "fetch-readme" $ do
    let payload = VersionedPackage mpPackage mpVersion
    result <- Hackage.request $ Hackage.getPackageReadme payload
    case result of
      Left e -> handleClientError e
      Right bodyText -> do
        readmeBody <- renderMarkdown ("README" <> show mpPackage) bodyText
        Update.updateReadme mpReleaseId (Just $ HTML.fromText readmeBody) Imported
  where
    handleClientError :: ClientError -> JobsRunner ()
    handleClientError e@(FailureResponse _ response)
      -- If the README simply doesn't exist, we skip it by marking the job as successful.
      | response.responseStatusCode == notFound404 = Update.updateReadme mpReleaseId Nothing Inexistent
      | response.responseStatusCode == gone410 = Update.updateReadme mpReleaseId Nothing Inexistent
      | otherwise = do
          Log.logAttention "Could not find tarball from hackage" $
            object
              [ "namespace" .= ("hackage" :: Text)
              , "package_name" .= mpPackage
              , "package_version" .= mpVersion
              , "release_id" .= mpReleaseId
              ]
          Arb.throwRetryable (Text.show e)
    handleClientError e = Arb.throwRetryable (Text.show e)

fetchTarball
  :: RequireCallStack
  => TarballJobPayload
  -> JobsRunner ()
fetchTarball TarballJobPayload{releaseId, packageName, packageVersion} = do
  localDomain "fetch-tarball" $ do
    mArchive <- Query.getReleaseTarballArchive releaseId
    content <- case mArchive of
      Just bs -> pure bs
      Nothing -> do
        let payload = VersionedPackage packageName packageVersion
        result <- Hackage.request $ Hackage.getPackageTarball payload
        case result of
          Right bs -> pure bs
          Left e -> handleClientError e
    mhash <- Update.insertTar packageName packageVersion.unIntAesonVersion content
    case mhash of
      Right hash ->
        logTrace
          ("Inserted tarball for " <> display packageName)
          (object ["release_id" .= releaseId, "root_hash" .= hash])
      Left err -> do
        logAttention_ $ "Failed to insert tarball for " <> display packageName
        throw err
  where
    handleClientError :: ClientError -> JobsRunner a
    handleClientError e@(FailureResponse _ response)
      | response.responseStatusCode == notFound404 = do
          Log.logAttention "Could not find tarball from hackage" $
            object
              [ "namespace" .= ("hackage" :: Text)
              , "package_name" .= packageName
              , "package_version" .= packageVersion
              , "release_id" .= releaseId
              ]
          Arb.throwPermanent "Package does not exist"
      | otherwise = do
          Log.logAttention "Could not fetch tarball from hackage" $
            object
              [ "namespace" .= ("hackage" :: Text)
              , "package_name" .= packageName
              , "package_version" .= packageVersion
              , "release_id" .= releaseId
              , "status_code" .= statusCode response.responseStatusCode
              ]
          Arb.throwRetryable (Text.show e)
    handleClientError e = Arb.throwRetryable (Text.show e)

fetchUploadInformation :: RequireCallStack => UploadInformationJobPayload -> JobsRunner ()
fetchUploadInformation payload@UploadInformationJobPayload{packageName, packageVersion, releaseId} =
  localDomain "fetch-upload-information" $ do
    logTrace "Fetching upload information" payload
    let requestPayload = VersionedPackage packageName packageVersion
    result <- Hackage.request $ Hackage.getPackageInfo requestPayload
    case result of
      Left e -> handleClientError e
      Right packageInfo ->
        if packageInfo.metadataRevision == 0
          then do
            Log.logTrace_ "No revision, using the upload time"
            Update.updateUploadTime releaseId packageInfo.uploadedAt
          else do
            Log.logTrace_ "Found a revision, querying the original package info"
            (Hackage.request $ Hackage.getPackageWithRevision requestPayload 0) >>= \case
              Right originalPackageInfo -> do
                Update.updateRevisionTime releaseId packageInfo.uploadedAt
                Update.updateUploadTime releaseId originalPackageInfo.uploadedAt
                Update.linkPackageUploaderToImportedRelease releaseId packageInfo.uploader
              Left e -> handleClientError e
  where
    handleClientError :: ClientError -> JobsRunner ()
    handleClientError e@(FailureResponse _ response)
      | response.responseStatusCode == notFound404 = do
          Log.logAttention "Error while getting release upload information" $
            object
              [ "namespace" .= ("hackage" :: Text)
              , "package_name" .= packageName
              , "package_version" .= packageVersion
              , "release_id" .= releaseId
              ]
          Arb.throwPermanent "Package does not exist"
      | otherwise = do
          Log.logAttention "Error while getting release upload information" $
            object
              [ "namespace" .= ("hackage" :: Text)
              , "package_name" .= packageName
              , "package_version" .= packageVersion
              , "release_id" .= releaseId
              ]
          Arb.throwRetryable (Text.show e)
    handleClientError e = do
      Log.logAttention "Error while getting release upload information" $
        object
          [ "namespace" .= ("hackage" :: Text)
          , "package_name" .= packageName
          , "package_version" .= packageVersion
          , "release_id" .= releaseId
          ]
      Arb.throwRetryable (Text.show e)

-- | This job fetches the deprecation list and inserts the appropriate metadata in the packages
fetchPackageDeprecationList :: RequireCallStack => JobsRunner ()
fetchPackageDeprecationList = do
  result <- Hackage.request Hackage.getDeprecatedPackages
  case result of
    Right deprecationList -> do
      deprecationList
        & Vector.map
          ( \DeprecatedPackage'{package, inFavourOf} ->
              DeprecatedPackage package (assignNamespace inFavourOf)
          )
        & Update.deprecatePackages
    Left e -> handleClientError e
  where
    handleClientError :: ClientError -> JobsRunner ()
    handleClientError e@(FailureResponse _ response)
      | response.responseStatusCode == notFound404 = do
          Log.logAttention "Error while getting deprecated packages" $
            object
              [ "error" .= Text.show e
              ]
          Arb.throwPermanent "Package does not exist"
      | otherwise =
          Arb.throwRetryable (Text.show e)
    handleClientError e = Arb.throwRetryable (Text.show e)

assignNamespace :: Vector PackageName -> PackageAlternatives
assignNamespace =
  PackageAlternatives . Vector.map (\p -> PackageAlternative (Namespace "hackage") p)

fetchReleaseDeprecationList :: RequireCallStack => PackageName -> Vector ReleaseId -> JobsRunner ()
fetchReleaseDeprecationList packageName releases = do
  result <- Hackage.request $ Hackage.getDeprecatedReleasesList packageName
  case result of
    Right deprecationList -> do
      releasesAndVersions <- Query.getVersionFromManyReleaseIds releases
      let (deprecatedVersions', preferredVersions') =
            Vector.unstablePartition
              ( \(_, v) ->
                  Vector.elem v deprecationList.deprecatedVersions
              )
              releasesAndVersions
      let deprecatedVersions =
            fmap (\(releaseId, _) -> (True, releaseId)) deprecatedVersions'
      let preferredVersions =
            fmap (\(releaseId, _) -> (False, releaseId)) preferredVersions'
      unless (Vector.null deprecatedVersions) $
        Update.setReleasesDeprecationMarker deprecatedVersions
      unless (Vector.null preferredVersions) $
        Update.setReleasesDeprecationMarker preferredVersions
    Left e -> handleClientError e
  where
    handleClientError :: ClientError -> JobsRunner ()
    handleClientError e@(FailureResponse _ response)
      | response.responseStatusCode == notFound404 = do
          Log.logAttention "Could not find package in remote repository" $
            object
              [ "namespace" .= ("hackage" :: Text)
              , "package_name" .= packageName
              ]
          Arb.throwPermanent "Package does not exist"
      | otherwise = do
          Log.logAttention "Could not fetch release deprecation list" $
            object
              [ "namespace" .= ("hackage" :: Text)
              , "package_name" .= packageName
              , "status_code" .= statusCode response.responseStatusCode
              , "error" .= Text.show e
              ]
          Arb.throwRetryable (Text.show e)
    handleClientError e = Arb.throwRetryable (Text.show e)

refreshIndex
  :: ( Concurrent :> es
     , DB :> es
     , Error ImportError :> es
     , FileSystem :> es
     , IOE :> es
     , Log :> es
     , Metrics AppMetrics :> es
     , Reader FloraEnv :> es
     , State (Set (Namespace, PackageName, Version)) :> es
     , Time :> es
     , TypedProcess :> es
     )
  => ArbS.SimpleEnv JobQueues
  -> Text
  -> FloraM es ()
refreshIndex env indexName = do
  runProcess_ $ shell "cabal update --project-file cabal.project.repositories"
  packagesPath <- getCabalPackagesDirectory
  mPackageIndex <- Query.getPackageIndexByName indexName
  case mPackageIndex of
    Nothing -> do
      Log.logAttention "Package index not found" $
        object ["package_index" .= indexName]
      error $ Text.unpack $ "Package index " <> indexName <> " not found in the database!"
    Just packageIndex -> do
      indexDependencies <- Query.getIndexDependencies packageIndex.packageIndexId
      Import.importFromArchive indexName indexDependencies packagesPath

      releasesWithoutReadme <- Query.getHackagePackageReleasesWithoutReadme
      liftIO $
        void $
          forkIO $
            Async.forConcurrently_
              releasesWithoutReadme
              (\(releaseId, version, packagename) -> scheduleReadmeJob env releaseId packagename version)

      hackageReleasesWithoutUploadInformation <- Query.getHackagePackageReleasesWithoutUploadInformation
      liftIO $
        void $
          forkIO $
            Async.forConcurrently_
              hackageReleasesWithoutUploadInformation
              (\(releaseId, version, packagename) -> scheduleUploadInformationJob env releaseId packagename version)

      releasesWithoutChangelog <- Query.getHackagePackageReleasesWithoutChangelog
      liftIO $
        void $
          forkIO $
            Async.forConcurrently_
              releasesWithoutChangelog
              (\(releaseId, version, packagename) -> scheduleChangelogJob env releaseId packagename version)

      packagesWithoutDeprecationInformation <- Query.getHackagePackagesWithoutReleaseDeprecationInformation
      liftIO $
        void $
          forkIO $ do
            Async.forConcurrently_
              packagesWithoutDeprecationInformation
              (\a -> scheduleReleaseDeprecationListJob env a)
            void $ scheduleRefreshLatestVersions env

      void $ liftIO $ scheduleRefreshIndex env indexName

getCabalPackagesDirectory :: FileSystem :> es => FloraM es FilePath
getCabalPackagesDirectory = do
  xdgPath <- FileSystem.getXdgDirectory FileSystem.XdgCache "/packages"
  xdgPathExists <- FileSystem.doesDirectoryExist xdgPath
  if xdgPathExists
    then pure xdgPath
    else do
      homeDir <- FileSystem.getHomeDirectory
      let legacyPackagesDirectory = homeDir </> ".cabal/packages"
      pure legacyPackagesDirectory
