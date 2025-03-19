module FloraJobs.Runner where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (Result (..), fromJSON, toJSON)
import Data.Function
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text
import Data.Text qualified as Text
import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Types.Version (Version)
import Effectful (Eff, IOE, type (:>))
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as FileSystem
import Effectful.Log
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Process.Typed
import Effectful.Prometheus
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Shared (State)
import Effectful.Time (Time)
import Log
import Network.HTTP.Types (gone410, notFound404, statusCode)
import OddJobs.Job (Job (..))
import Servant.Client (ClientError (..))
import Servant.Client.Core (ResponseF (..))
import System.FilePath

import Data.Text.HTML qualified as HTML
import Effectful.Poolboy (Poolboy)
import Flora.Environment.Env
import Flora.Import.Package (coreLibraries, persistImportOutput)
import Flora.Import.Package.Bulk qualified as Import
import Flora.Model.BlobIndex.Update qualified as Update
import Flora.Model.BlobStore.API
import Flora.Model.Job
import Flora.Model.Package.Types
import Flora.Model.Package.Update qualified as Update
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Model.Release.Update qualified as Update
import Flora.Model.User (User (..))
import Flora.Model.User.Query qualified as Query
import FloraJobs.Render (renderMarkdown)
import FloraJobs.ThirdParties.Hackage.API
  ( HackagePackageInfo (..)
  , HackagePreferredVersions (..)
  , VersionedPackage (..)
  )
import FloraJobs.ThirdParties.Hackage.Client qualified as Hackage
import FloraJobs.Types

runner :: Job -> JobsRunner ()
runner job = localDomain "job-runner" $
  case fromJSON (jobPayload job) of
    Error str -> logMessage LogAttention "decode error" (toJSON str)
    Success val -> case val of
      FetchReadme x -> makeReadme x
      FetchTarball x -> fetchTarball x
      FetchUploadTime x -> fetchUploadTime x
      FetchChangelog x -> fetchChangeLog x
      ImportPackage x -> persistImportOutput x
      FetchPackageDeprecationList -> fetchPackageDeprecationList
      FetchReleaseDeprecationList packageName releases -> fetchReleaseDeprecationList packageName releases
      RefreshLatestVersions -> Update.refreshLatestVersions
      RefreshIndex indexName -> refreshIndex indexName

fetchChangeLog :: ChangelogJobPayload -> JobsRunner ()
fetchChangeLog payload@ChangelogJobPayload{packageName, packageVersion, releaseId} =
  localDomain "fetch-changelog" $ do
    Log.logInfo "Fetching CHANGELOG" payload
    let requestPayload = VersionedPackage packageName packageVersion
    result <- Hackage.request $ Hackage.getPackageChangelog requestPayload
    case result of
      Left e@(FailureResponse _ response)
        -- If the CHANGELOG simply doesn't exist, we skip it by marking the job as successful.
        | response.responseStatusCode == notFound404 -> Update.updateChangelog releaseId Nothing Inexistent
        | response.responseStatusCode == gone410 -> Update.updateChangelog releaseId Nothing Inexistent
        | otherwise -> throw e
      Left e -> throw e
      Right bodyText -> do
        logInfo ("got a changelog for package " <> display packageName) (object ["release_id" .= releaseId])
        changelogBody <- renderMarkdown ("CHANGELOG" <> show packageName) bodyText
        Update.updateChangelog releaseId (Just $ HTML.fromText changelogBody) Imported

makeReadme :: ReadmeJobPayload -> JobsRunner ()
makeReadme pay@ReadmeJobPayload{..} =
  localDomain "fetch-readme" $ do
    logInfo "Fetching README" pay
    let payload = VersionedPackage mpPackage mpVersion
    gewt <- Hackage.request $ Hackage.getPackageReadme payload
    case gewt of
      Left e@(FailureResponse _ response)
        -- If the README simply doesn't exist, we skip it by marking the job as successful.
        | response.responseStatusCode == notFound404 -> Update.updateReadme mpReleaseId Nothing Inexistent
        | response.responseStatusCode == gone410 -> Update.updateReadme mpReleaseId Nothing Inexistent
        | otherwise -> throw e
      Left e -> throw e
      Right bodyText -> do
        logInfo ("got a readme for package " <> display mpPackage) (object ["release_id" .= mpReleaseId])
        readmeBody <- renderMarkdown ("README" <> show mpPackage) bodyText
        Update.updateReadme mpReleaseId (Just $ HTML.fromText readmeBody) Imported

fetchTarball
  :: ( BlobStoreAPI :> es
     , DB :> es
     , IOE :> es
     , Log :> es
     , Reader JobsRunnerEnv :> es
     )
  => TarballJobPayload
  -> Eff es ()
fetchTarball pay@TarballJobPayload{..} = do
  localDomain "fetch-tarball" $ do
    mArchive <- Query.getReleaseTarballArchive releaseId
    content <- case mArchive of
      Just bs -> pure bs
      Nothing -> do
        logInfo "Fetching tarball" pay
        let payload = VersionedPackage{..}
        result <- Hackage.request $ Hackage.getPackageTarball payload
        case result of
          Right bs -> pure bs
          Left e@(FailureResponse _ response) -> do
            logAttention "Could not fetch tarball from hackage" $
              object
                [ "package" .= display payload.package
                , "status_code" .= statusCode response.responseStatusCode
                ]
            throw e
          Left e -> throw e
    mhash <- Update.insertTar package (unIntAesonVersion version) content
    case mhash of
      Right hash ->
        logInfo
          ("Inserted tarball for " <> display package)
          (object ["release_id" .= releaseId, "root_hash" .= hash])
      Left err -> do
        logAttention_ $ "Failed to insert tarball for " <> display package
        throw err

fetchUploadTime :: UploadTimeJobPayload -> JobsRunner ()
fetchUploadTime payload@UploadTimeJobPayload{packageName, packageVersion, releaseId} =
  localDomain "fetch-upload-time" $ do
    logInfo "Fetching upload time" payload
    let requestPayload = VersionedPackage packageName packageVersion
    packageInfo <- liftIO $ Hackage.getPackageInfo requestPayload
    if packageInfo.metadataRevision == 0
      then do
        Log.logInfo_ "No revision, using the upload time"
        Update.updateUploadTime releaseId packageInfo.uploadedAt
      else do
        Log.logInfo_ "Found a revision, querying the original package info"
        originalPackageInfo <- liftIO $ Hackage.getPackageWithRevision requestPayload 0
        Update.updateRevisionTime releaseId packageInfo.uploadedAt
        Update.updateUploadTime releaseId originalPackageInfo.uploadedAt

-- | This job fetches the deprecation list and inserts the appropriate metadata in the packages
fetchPackageDeprecationList :: JobsRunner ()
fetchPackageDeprecationList = do
  result <- Hackage.request Hackage.getDeprecatedPackages
  case result of
    Right deprecationList -> do
      logInfo_ "Deprecation List retrieved"
      deprecationList
        & Vector.map
          ( \DeprecatedPackage'{package, inFavourOf} ->
              DeprecatedPackage package (assignNamespace inFavourOf)
          )
        & Update.deprecatePackages
    Left e@(FailureResponse _ response) -> do
      logAttention "Could not fetch package deprecation list from Hackage" $
        object
          [ "status_code" .= statusCode response.responseStatusCode
          ]
      throw e
    Left e -> throw e

fetchReleaseDeprecationList :: PackageName -> Vector ReleaseId -> JobsRunner ()
fetchReleaseDeprecationList packageName releases = do
  result <- Hackage.request $ Hackage.getDeprecatedReleasesList packageName
  case result of
    Right deprecationList -> do
      logInfo "Release deprecation list retrieved" $
        object ["package" .= display packageName]
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
    Left e@(FailureResponse _ response) -> do
      logAttention "Could not fetch release deprecation list from Hackage" $
        object
          [ "package" .= display packageName
          , "status_code" .= statusCode response.responseStatusCode
          ]
      throw e
    Left e -> throw e

assignNamespace :: Vector PackageName -> PackageAlternatives
assignNamespace =
  PackageAlternatives
    . Vector.map
      ( \p ->
          if Set.member p coreLibraries
            then PackageAlternative (Namespace "haskell") p
            else PackageAlternative (Namespace "hackage") p
      )

refreshIndex
  :: ( DB :> es
     , FileSystem :> es
     , IOE :> es
     , Log :> es
     , Metrics AppMetrics :> es
     , Poolboy :> es
     , State (Set (Namespace, PackageName, Version)) :> es
     , Time :> es
     , TypedProcess :> es
     )
  => Text
  -> Eff es ()
refreshIndex indexName = do
  let repoPath =
        if indexName == "hackage"
          then "hackage.haskell.org"
          else Text.unpack indexName
  runProcess_ $ shell "cabal update --project-file cabal.project.repositories"
  user <- fromJust <$> Query.getUserByUsername "hackage-user"
  packagesPath <- getCabalPackagesDirectory
  let path = packagesPath </> repoPath </> "01-index.tar.gz"
  mPackageIndex <- Query.getPackageIndexByName indexName
  case mPackageIndex of
    Nothing -> do
      Log.logAttention "Package index not found" $
        object ["package_index" .= indexName]
      error $ Text.unpack $ "Package index " <> indexName <> " not found in the database!"
    Just _ ->
      Import.importFromIndex user.userId indexName path

getCabalPackagesDirectory :: FileSystem :> es => Eff es FilePath
getCabalPackagesDirectory = do
  xdgPath <- FileSystem.getXdgDirectory FileSystem.XdgCache "/packages"
  xdgPathExists <- FileSystem.doesDirectoryExist xdgPath
  if xdgPathExists
    then pure xdgPath
    else do
      homeDir <- FileSystem.getHomeDirectory
      let legacyPackagesDirectory = homeDir </> ".cabal/packages"
      pure legacyPackagesDirectory
