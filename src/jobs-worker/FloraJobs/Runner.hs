module FloraJobs.Runner
  ( runner
  ) where

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
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.Log hiding (LogLevel)
import Effectful.Process.Typed
import Effectful.Prometheus
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Shared (State)
import Effectful.Time (Time)
import Effectful.Trace (Trace)
import Log hiding (LogLevel)
import Network.HTTP.Types (gone410, notFound404, statusCode)
import RequireCallStack
import Servant.Client (ClientError (..))
import Servant.Client.Core (ResponseF (..))
import System.FilePath

import Data.Text.HTML qualified as HTML
import Flora.Database
import Flora.Environment.Env
import Flora.Import.Package (persistImportOutput)
import Flora.Import.Package.Bulk.Archive qualified as Import
import Flora.Import.Types
import Flora.Model.BlobIndex.Update qualified as Update
import Flora.Model.Job
import Flora.Model.Package.Guard (guardThatPackageExists)
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Model.Package.Update qualified as Update
import Flora.Model.PackageIndex.Guard
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageMaintainer.Types
import Flora.Model.PackageMaintainer.Update qualified as Update
import Flora.Model.PackageUploader.Guard
import Flora.Model.PackageUploader.Types
import Flora.Model.PackageUploader.Update qualified as Update
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Model.Release.Update qualified as Update
import Flora.Monad
import FloraJobs.Environment
import FloraJobs.Render (renderMarkdown)
import FloraJobs.Scheduler
import FloraJobs.ThirdParties.Hackage.API
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
  RefreshLatestVersions -> do
    FloraJobsEnv{pool} <- Reader.ask
    withReadWritePool pool Update.refreshLatestVersions
  RefreshIndex indexName -> refreshIndex env indexName
  FetchPackageMaintainers packageName -> fetchPackageMaintainers packageName
  FetchPackageUploaders -> fetchPackageUploaders

fetchChangeLog :: RequireCallStack => ChangelogJobPayload -> JobsRunner ()
fetchChangeLog ChangelogJobPayload{packageName, packageVersion, releaseId} =
  localDomain "fetch-changelog" $ do
    FloraJobsEnv{pool} <- Reader.ask
    let requestPayload = VersionedPackage packageName packageVersion
    result <- Hackage.request $ Hackage.getPackageChangelog requestPayload
    case result of
      Left e -> handleClientError e
      Right bodyText -> do
        changelogBody <- renderMarkdown ("CHANGELOG" <> show packageName) bodyText
        withReadWritePool pool $ Update.updateChangelog releaseId (Just $ HTML.fromText changelogBody) Imported
  where
    handleClientError :: ClientError -> JobsRunner ()
    handleClientError e@(FailureResponse _ response)
      -- If the CHANGELOG simply doesn't exist, we skip it by marking the job as successful.
      | response.responseStatusCode == notFound404 = do
          FloraJobsEnv{pool} <- Reader.ask
          withReadWritePool pool $ Update.updateChangelog releaseId Nothing Inexistent
      | response.responseStatusCode == gone410 = do
          FloraJobsEnv{pool} <- Reader.ask
          withReadWritePool pool $ Update.updateChangelog releaseId Nothing Inexistent
      | otherwise = Arb.throwRetryable (Text.show e)
    handleClientError e = Arb.throwRetryable (Text.show e)

makeReadme :: RequireCallStack => ReadmeJobPayload -> JobsRunner ()
makeReadme ReadmeJobPayload{mpPackage, mpReleaseId, mpVersion} =
  localDomain "fetch-readme" $ do
    FloraJobsEnv{pool} <- Reader.ask
    let payload = VersionedPackage mpPackage mpVersion
    result <- Hackage.request $ Hackage.getPackageReadme payload
    case result of
      Left e -> handleClientError e
      Right bodyText -> do
        readmeBody <- renderMarkdown ("README" <> show mpPackage) bodyText
        withReadWritePool pool $ Update.updateReadme mpReleaseId (Just $ HTML.fromText readmeBody) Imported
  where
    handleClientError :: ClientError -> JobsRunner ()
    handleClientError e@(FailureResponse _ response)
      -- If the README simply doesn't exist, we skip it by marking the job as successful.
      | response.responseStatusCode == notFound404 = do
          FloraJobsEnv{pool} <- Reader.ask
          withReadWritePool pool $ Update.updateReadme mpReleaseId Nothing Inexistent
      | response.responseStatusCode == gone410 = do
          FloraJobsEnv{pool} <- Reader.ask
          withReadWritePool pool $ Update.updateReadme mpReleaseId Nothing Inexistent
      | otherwise = do
          Log.logAttention "Could not get README hackage" $
            object
              [ "namespace" .= ("hackage" :: Text)
              , "package_name" .= mpPackage
              , "package_version" .= mpVersion
              , "release_id" .= mpReleaseId
              , "error" .= Text.show e
              ]
          Arb.throwRetryable (Text.show e)
    handleClientError e = Arb.throwRetryable (Text.show e)

fetchTarball
  :: RequireCallStack
  => TarballJobPayload
  -> JobsRunner ()
fetchTarball TarballJobPayload{releaseId, namespace, packageName, packageVersion} = do
  localDomain "fetch-tarball" $ do
    FloraJobsEnv{pool} <- Reader.ask
    mArchive <- withReadOnlyPool pool $ Query.getReleaseTarballArchive releaseId
    content <- case mArchive of
      Just bs -> pure bs
      Nothing -> do
        let payload = VersionedPackage packageName packageVersion
        result <- Hackage.request $ Hackage.getPackageTarball payload
        case result of
          Right bs -> pure bs
          Left e -> handleClientError e
    mhash <- withReadWritePool pool $ Update.insertTar namespace packageName packageVersion.unIntAesonVersion content
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
      | response.responseStatusCode == gone410 = do
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
    FloraJobsEnv{pool} <- Reader.ask
    logTrace "Fetching upload information" payload
    let requestPayload = VersionedPackage packageName packageVersion
    result <- Hackage.request $ Hackage.getPackageInfo requestPayload
    case result of
      Left e -> handleClientError e
      Right packageInfo ->
        if packageInfo.metadataRevision == 0
          then do
            withReadWritePool pool $ Update.updateUploadTime releaseId packageInfo.uploadedAt
            withReadWritePool pool $ withReadOnlyPool pool $ Update.linkPackageUploaderToImportedRelease releaseId packageInfo.uploader
          else do
            Hackage.request (Hackage.getPackageWithRevision requestPayload 0) >>= \case
              Right originalPackageInfo -> do
                withReadWritePool pool $ Update.updateRevisionTime releaseId packageInfo.uploadedAt
                withReadWritePool pool $ Update.updateUploadTime releaseId originalPackageInfo.uploadedAt
                withReadWritePool pool $ withReadOnlyPool pool $ Update.linkPackageUploaderToImportedRelease releaseId packageInfo.uploader
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
      | response.responseStatusCode == gone410 = do
          Log.logAttention "Error while getting release upload information" $
            object
              [ "namespace" .= ("hackage" :: Text)
              , "package_name" .= packageName
              , "package_version" .= packageVersion
              , "release_id" .= releaseId
              ]
          Arb.throwPermanent "Package is gone"
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
  FloraJobsEnv{pool} <- Reader.ask
  result <- Hackage.request Hackage.getDeprecatedPackages
  case result of
    Right deprecationList -> do
      deprecationList
        & Vector.map
          ( \DeprecatedPackage'{package, inFavourOf} ->
              DeprecatedPackage package (assignNamespace inFavourOf)
          )
        & withReadWritePool pool . Update.deprecatePackages
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
  FloraJobsEnv{pool} <- Reader.ask
  result <- Hackage.request $ Hackage.getDeprecatedReleasesList packageName
  case result of
    Right deprecationList -> do
      releasesAndVersions <- withReadOnlyPool pool $ Query.getVersionFromManyReleaseIds releases
      let (deprecatedVersions', _) =
            Vector.unstablePartition
              ( \(_, v) ->
                  Vector.elem v deprecationList.deprecatedVersions
              )
              releasesAndVersions
      let deprecatedVersions =
            fmap (\(releaseId, _) -> (True, releaseId)) deprecatedVersions'
      unless (Vector.null deprecatedVersions) $
        withReadWritePool pool $
          Update.setReleasesDeprecationMarker deprecatedVersions
    Left e -> handleClientError e
  where
    handleClientError :: ClientError -> JobsRunner ()
    handleClientError e@(FailureResponse _ response)
      | response.responseStatusCode == notFound404 = do
          Log.logAttention "Could not find package in remote repository" $
            object
              [ "namespace" .= ("hackage" :: Text)
              , "package_name" .= packageName
              , "status_code" .= statusCode response.responseStatusCode
              , "error" .= Text.show e
              ]
          Arb.throwPermanent "Package does not exist"
      | response.responseStatusCode == gone410 = do
          Log.logAttention "Could not find package in remote repository" $
            object
              [ "namespace" .= ("hackage" :: Text)
              , "package_name" .= packageName
              , "status_code" .= statusCode response.responseStatusCode
              , "error" .= Text.show e
              ]
          Arb.throwPermanent "Package is gone"
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
     , Error ImportError :> es
     , FileSystem :> es
     , IOE :> es
     , Log :> es
     , Metrics AppMetrics :> es
     , Reader FloraEnv :> es
     , State (Set (Namespace, PackageName, Version)) :> es
     , Time :> es
     , Tracer :> es
     , TypedProcess :> es
     )
  => ArbS.SimpleEnv JobQueues
  -> Text
  -> FloraM es ()
refreshIndex env indexName = do
  FloraEnv{pool} <- Reader.ask
  runProcess_ $ shell "cabal update --project-file cabal.project.repositories"
  packagesPath <- getCabalPackagesDirectory
  mPackageIndex <- withReadOnlyPool pool $ Query.getPackageIndexByName indexName
  case mPackageIndex of
    Nothing -> do
      Log.logAttention "Package index not found" $
        object ["package_index" .= indexName]
      error $ Text.unpack $ "Package index " <> indexName <> " not found in the database!"
    Just packageIndex -> do
      indexDependencies <- withReadOnlyPool pool $ Query.getIndexDependencies packageIndex.packageIndexId
      Import.importFromArchive indexName indexDependencies packagesPath

      releasesWithoutReadme <- withReadOnlyPool pool Query.getHackagePackageReleasesWithoutReadme
      liftIO $
        void $
          forkIO $
            Async.forConcurrently_
              releasesWithoutReadme
              (\(releaseId, version, packagename) -> scheduleReadmeJob env releaseId packagename version)

      hackageReleasesWithoutUploadInformation <- withReadOnlyPool pool Query.getHackagePackageReleasesWithoutUploadInformation
      liftIO $
        void $
          forkIO $
            Async.forConcurrently_
              hackageReleasesWithoutUploadInformation
              (\(releaseId, version, packagename) -> scheduleUploadInformationJob env releaseId packagename version)

      releasesWithoutChangelog <- withReadOnlyPool pool Query.getHackagePackageReleasesWithoutChangelog
      liftIO $
        void $
          forkIO $
            Async.forConcurrently_
              releasesWithoutChangelog
              (\(releaseId, version, packagename) -> scheduleChangelogJob env releaseId packagename version)

      packagesWithoutDeprecationInformation <- withReadOnlyPool pool Query.getHackagePackagesWithoutReleaseDeprecationInformation
      liftIO $
        void $
          forkIO $ do
            Async.forConcurrently_
              packagesWithoutDeprecationInformation
              (\a -> scheduleReleaseDeprecationListJob env a)
            void $ scheduleRefreshLatestVersions env

      packagesWithoutMaintainerInformation <- withReadOnlyPool pool Query.getPackagesWithoutMaintainersInformation
      liftIO $
        void $
          forkIO $
            Async.forConcurrently_
              packagesWithoutMaintainerInformation
              (\(_namespace, packageName) -> schedulePackageMaintainersListJob env packageName)

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

fetchPackageMaintainers
  :: RequireCallStack
  => PackageName
  -> JobsRunner ()
fetchPackageMaintainers packageName = do
  localDomain "fetch-package-maintainers" $ do
    FloraJobsEnv{pool} <- Reader.ask
    packageIndex <- withReadOnlyPool pool $ guardThatPackageIndexExists "hackage" (Error.throwError (CouldNotFindPackageIndex "hackage"))
    Hackage.request (Hackage.getPackageMaintainers packageName) >>= \case
      Left e -> handleClientError e
      Right (HackagePackageMaintainers maintainers) -> do
        let namespace = Namespace packageIndex.repository
        package <-
          withReadOnlyPool pool $
            guardThatPackageExists
              namespace
              packageName
              (\_ _ -> Error.throwError (CouldNotFindPackage namespace packageName))
        packageUploaders <- forM (Vector.toList maintainers) $ \(HackagePackageMaintainer username) ->
          withReadOnlyPool pool $
            guardThatPackageUploaderExists
              username
              packageIndex.packageIndexId
              (Error.throwError (CouldNotFindPackageUploader username namespace))
        packageMaintainerDAOs <- forM packageUploaders $ \packageUploader ->
          mkPackageMaintainer
            packageUploader.packageUploaderId
            package.packageId
        withReadWritePool pool $ Update.insertPackageMaintainers packageMaintainerDAOs
  where
    handleClientError :: ClientError -> JobsRunner a
    handleClientError e@(FailureResponse _ response)
      | response.responseStatusCode == notFound404 = do
          Log.logAttention "Could not find package on hackage" $
            object
              [ "namespace" .= ("hackage" :: Text)
              , "package_name" .= packageName
              ]
          Arb.throwPermanent "Package does not exist"
      | response.responseStatusCode == gone410 = do
          Log.logAttention "Could not find package on hackage" $
            object
              [ "namespace" .= ("hackage" :: Text)
              , "package_name" .= packageName
              ]
          Arb.throwPermanent "Package does not exist"
      | otherwise = do
          Log.logAttention "Could not fetch package on hackage" $
            object
              [ "namespace" .= ("hackage" :: Text)
              , "package_name" .= packageName
              , "status_code" .= statusCode response.responseStatusCode
              ]
          Arb.throwRetryable (Text.show e)
    handleClientError e = Arb.throwRetryable (Text.show e)

fetchPackageUploaders :: RequireCallStack => JobsRunner ()
fetchPackageUploaders = do
  localDomain "fetch-package-uploaders" $ do
    FloraJobsEnv{pool} <- Reader.ask
    packageIndex <- withReadOnlyPool pool $ guardThatPackageIndexExists "hackage" (Error.throwError (CouldNotFindPackageIndex "hackage"))
    Hackage.request Hackage.listHackageUsers >>= \case
      Left e -> handleClientError e
      Right users -> do
        forM_ users $ \user -> do
          dao <- mkPackageUploaderDAO user.username packageIndex.packageIndexId Nothing
          withReadWritePool pool $ Update.insertMaybeExistingPackageUploader dao
  where
    handleClientError :: ClientError -> JobsRunner a
    handleClientError e = Arb.throwRetryable (Text.show e)
