module FloraJobs.Runner where

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (Result (..), fromJSON, toJSON)
import Data.Function
import Data.Set qualified as Set
import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful.PostgreSQL.Transact.Effect
import Log
import Network.HTTP.Types (gone410, notFound404, statusCode)
import OddJobs.Job (Job (..))
import Servant.Client (ClientError (..))
import Servant.Client.Core (ResponseF (..))
import System.Process.Typed qualified as System

import Flora.Import.Package (coreLibraries, persistImportOutput, withWorkerDbPool)
import Flora.Model.Job
import Flora.Model.Package.Types
import Flora.Model.Package.Update qualified as Update
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Model.Release.Update qualified as Update
import FloraJobs.Render (renderMarkdown)
import FloraJobs.Scheduler
import FloraJobs.ThirdParties.Hackage.API (HackagePackageInfo (..), HackagePreferredVersions (..), VersionedPackage (..))
import FloraJobs.ThirdParties.Hackage.Client qualified as Hackage
import FloraJobs.Types

fetchNewIndex :: JobsRunner ()
fetchNewIndex =
  localDomain "index-import" $ do
    logInfo_ "Fetching new index"
    System.runProcess_ "cabal update"
    System.runProcess_ "cp ~/.cabal/packages/hackage.haskell.org/01-index.tar 01-index/"
    System.runProcess_ "cd 01-index && tar -xf 01-index.tar"
    System.runProcess_ "make import-from-hackage"
    logInfo_ "New index processed"
    releases <- Query.getPackageReleasesWithoutReadme
    pool <- getPool
    liftIO $
      forkIO $
        forM_
          releases
          ( \(releaseId, version, packagename) -> scheduleReadmeJob pool releaseId packagename version
          )
    liftIO $ void $ scheduleIndexImportJob pool

runner :: Job -> JobsRunner ()
runner job = localDomain "job-runner" $
  case fromJSON (jobPayload job) of
    Error str -> logMessage LogAttention "decode error" (toJSON str)
    Success val -> case val of
      FetchReadme x -> makeReadme x
      FetchUploadTime x -> fetchUploadTime x
      FetchChangelog x -> fetchChangeLog x
      ImportHackageIndex _ -> fetchNewIndex
      ImportPackage x ->
        withWorkerDbPool $ \wq ->
          persistImportOutput wq x
      FetchPackageDeprecationList -> fetchPackageDeprecationList
      FetchReleaseDeprecationList packageName releases ->
        fetchReleaseDeprecationList packageName releases
      RefreshLatestVersions ->
        Update.refreshLatestVersions

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
        let changelogBody = renderMarkdown ("CHANGELOG" <> show packageName) bodyText
        Update.updateChangelog releaseId (Just $ MkTextHtml changelogBody) Imported

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
        let readmeBody = renderMarkdown ("README" <> show mpPackage) bodyText
        Update.updateReadme mpReleaseId (Just $ MkTextHtml readmeBody) Imported

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
