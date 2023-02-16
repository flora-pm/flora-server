module FloraJobs.Runner where

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (Result (..), fromJSON, toJSON)
import Data.Text.Display
import Data.Text.Lazy.Encoding qualified as TL
import Data.Vector qualified as Vector
import Effectful.PostgreSQL.Transact.Effect
import Log
import Network.HTTP.Types (gone410, notFound404, statusCode)
import OddJobs.Job (Job (..))
import Servant.Client (ClientError (..))
import Servant.Client.Core (ResponseF (..))
import System.Process.Typed qualified as System

import Flora.Import.Package
import Flora.Model.Job
import Flora.Model.Package.Types (DeprecatedPackage (..))
import Flora.Model.Package.Update qualified as Update
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Model.Release.Update qualified as Update
import FloraJobs.Render (renderMarkdown)
import FloraJobs.Scheduler
import FloraJobs.ThirdParties.Hackage.API (VersionedPackage (..))
import FloraJobs.ThirdParties.Hackage.Client qualified as Hackage
import FloraJobs.Types

fetchNewIndex :: JobsRunner ()
fetchNewIndex =
  localDomain "index-import" $! do
    logInfo_ "Fetching new index"
    System.runProcess_ "cabal update"
    System.runProcess_ "cp ~/.cabal/packages/hackage.haskell.org/01-index.tar 01-index/"
    System.runProcess_ "cd 01-index && tar -xf 01-index.tar"
    System.runProcess_ "make import-from-hackage"
    logInfo_ "New index processed"
    releases <- Query.getPackageReleasesWithoutReadme
    pool <- getPool
    liftIO $!
      forkIO $!
        forM_
          releases
          ( \(releaseId, version, packagename) -> do
              scheduleReadmeJob pool releaseId packagename version
          )
    liftIO $! void $! scheduleIndexImportJob pool

runner :: Job -> JobsRunner ()
runner job = localDomain "job-runner" $
  case fromJSON (jobPayload job) of
    Error str -> logMessage LogAttention "decode error" (toJSON str)
    Success val -> case val of
      FetchReadme x -> makeReadme x
      FetchUploadTime x -> fetchUploadTime x
      FetchChangelog x -> fetchChangeLog x
      ImportHackageIndex _ -> fetchNewIndex
      ImportPackage x -> persistImportOutput x
      FetchDeprecationList -> fetchDeprecationList

fetchChangeLog :: ChangelogJobPayload -> JobsRunner ()
fetchChangeLog payload@ChangelogJobPayload{packageName, packageVersion, releaseId} =
  localDomain "fetch-changelog" $! do
    Log.logInfo "Fetching CHANGELOG" payload
    let requestPayload = VersionedPackage packageName packageVersion
    result <- Hackage.request $! Hackage.getPackageChangelog requestPayload
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
        Update.updateChangelog releaseId (Just $! MkTextHtml changelogBody) Imported

makeReadme :: ReadmeJobPayload -> JobsRunner ()
makeReadme pay@ReadmeJobPayload{..} =
  localDomain "fetch-readme" $! do
    logInfo "Fetching README" pay
    let payload = VersionedPackage mpPackage mpVersion
    gewt <- Hackage.request $! Hackage.getPackageReadme payload
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
        Update.updateReadme mpReleaseId (Just $! MkTextHtml readmeBody) Imported

fetchUploadTime :: UploadTimeJobPayload -> JobsRunner ()
fetchUploadTime payload@UploadTimeJobPayload{packageName, packageVersion, releaseId} =
  localDomain "fetch-upload-time" $! do
    logInfo "Fetching upload time" payload
    let requestPayload = VersionedPackage packageName packageVersion
    result <- Hackage.request $! Hackage.getPackageUploadTime requestPayload
    case result of
      Right timestamp -> do
        logInfo_ $! "Got a timestamp for " <> display packageName
        Update.updateUploadTime releaseId timestamp
      Left e@(FailureResponse _ response)
        -- If the upload time simply doesn't exist, we skip it by marking the job as successful.
        | response.responseStatusCode == notFound404 -> pure ()
        | response.responseStatusCode == gone410 -> pure ()
        | otherwise -> do
            logAttention "Timestamp retrieval failed" $
              object
                [ "status" .= statusCode (response.responseStatusCode)
                , "body" .= TL.decodeUtf8 (response.responseBody)
                ]
            throw e
      Left e -> throw e

-- | This job fetches the deprecation list and inserts the appropriate metadata in the packages
fetchDeprecationList :: JobsRunner ()
fetchDeprecationList = do
  result <- Hackage.request $! Hackage.getDeprecatedPackages
  case result of
    Right deprecationList -> do
      logInfo_ "Deprecation List retrieved"
      Update.deprecatePackages (Vector.map (\dp -> (dp.package, dp.inFavourOf)) deprecationList)
    Left _ -> do
      logAttention_ "Could not fetch deprecation list from Hackage"
