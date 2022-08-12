{-# LANGUAGE QuasiQuotes #-}

-- | Represents the various jobs that can be run
module Flora.OddJobs
  ( scheduleReadmeJob
  , scheduleUploadTimeJob
  , scheduleIndexImportJob
  , checkIfIndexImportJobIsNotRunning
  , jobTableName
  , runner

    -- * exposed for testing

  --   prefer using smart constructors.
  , ReadmePayload (..)
  , FloraOddJobs (..)
  , IntAesonVersion (..)
  )
where

import qualified Commonmark
import Control.Exception
import Data.Aeson (Result (..), fromJSON, toJSON)
import Data.Pool
import Data.Text
import Data.Text.Display
import qualified Data.Text.Lazy as TL
import qualified Database.PostgreSQL.Simple as PG
import Distribution.Types.Version
import Effectful.Log (localDomainEff', logMessageEff')
import Log
import qualified Lucid
import Network.HTTP.Types (notFound404, statusCode)
import OddJobs.Job (Job (..), createJob, scheduleJob)
import Optics.Core
import Servant.Client (ClientError (..))
import Servant.Client.Core (ResponseF (..))

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as Time
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL.Transact.Effect
import Flora.Model.Package
import Flora.Model.Release.Types
import Flora.Model.Release.Update (updateReadme)
import qualified Flora.Model.Release.Update as Update
import Flora.OddJobs.Types
import Flora.ThirdParties.Hackage.API (VersionedPackage (..))
import qualified Flora.ThirdParties.Hackage.Client as Hackage
import qualified System.Process.Typed as System

scheduleReadmeJob :: Pool PG.Connection -> ReleaseId -> PackageName -> Version -> IO Job
scheduleReadmeJob pool rid package version =
  withResource pool $ \res ->
    createJob
      res
      jobTableName
      (MkReadme $ MkReadmePayload package rid $ MkIntAesonVersion version)

scheduleUploadTimeJob :: Pool PG.Connection -> ReleaseId -> PackageName -> Version -> IO Job
scheduleUploadTimeJob pool releaseId packageName version = do
  withResource pool $ \res ->
    createJob
      res
      jobTableName
      (FetchUploadTime $ FetchUploadTimePayload packageName releaseId (MkIntAesonVersion version))

scheduleIndexImportJob :: Pool PG.Connection -> IO Job
scheduleIndexImportJob pool = do
  liftIO $ withResource pool $ \conn -> do
    t <- Time.getCurrentTime
    let runAt = Time.addUTCTime Time.nominalDay t
    scheduleJob
      conn
      jobTableName
      (ImportHackageIndex ImportHackageIndexPayload)
      runAt

checkIfIndexImportJobIsNotRunning :: JobsRunner Bool
checkIfIndexImportJobIsNotRunning = do
  Log.logTrace_ "Checking if the index import job is not running…"
  (result :: Maybe (Only Int)) <-
    dbtToEff $
      queryOne_
        Select
        [sql|
              select count(*)
              from "oddjobs"
              where payload ->> 'tag' = 'ImportHackageIndex'
      |]
  case result of
    Nothing -> do
      Log.logTrace_ "Index import job is running"
      pure True
    Just (Only 0) -> do
      Log.logTrace_ "Index import job is running"
      pure True
    Just (Only 1) -> do
      Log.logTrace_ "Index import job is running"
      pure True
    _ -> do
      Log.logTrace_ "Index import job not running"
      pure False

makeReadme :: ReadmePayload -> JobsRunner ()
makeReadme pay@MkReadmePayload{..} = localDomain ("for-package " <> display mpPackage) $ do
  logTrace "Fetching README" pay
  let payload = VersionedPackage mpPackage mpVersion
  gewt <- Hackage.request $ Hackage.getPackageReadme payload
  case gewt of
    Left e@(FailureResponse _ response) -> do
      -- If the README simply doesn't exist, we skip it by marking it as successful.
      if response ^. #responseStatusCode == notFound404
        then updateReadme mpReleaseId Nothing
        else throw e
    Left e -> throw e
    Right bodyText -> do
      logTrace ("got a body for package " <> display mpPackage) (object ["release_id" .= mpReleaseId])

      htmlTxt <- do
        -- let extensions = emojiSpec
        -- Commonmark.commonmarkWith extensions ("readme " <> show mpPackage) bodyText
        pure (Commonmark.commonmark ("readme " <> show mpPackage) bodyText)
          >>= \case
            Left exception -> throw (MarkdownFailed exception)
            Right (y :: Commonmark.Html ()) -> pure $ Commonmark.renderHtml y

      let readmeBody :: Lucid.Html ()
          readmeBody = Lucid.toHtmlRaw @Text $ TL.toStrict htmlTxt

      Update.updateReadme mpReleaseId (Just $ MkTextHtml readmeBody)

fetchUploadTime :: FetchUploadTimePayload -> JobsRunner ()
fetchUploadTime payload@FetchUploadTimePayload{packageName, packageVersion, releaseId} = localDomain "fetch-upload-time" $ do
  logTrace "Fetching upload time" payload
  let requestPayload = VersionedPackage packageName packageVersion
  result <- Hackage.request $ Hackage.getPackageUploadTime requestPayload
  case result of
    Right timestamp -> do
      logTrace_ $ "Got a timestamp for " <> display packageName
      Update.updateUploadTime releaseId timestamp
    Left e@(FailureResponse _ response) -> do
      logAttention "Timestamp retrieval failed" $
        object
          [ "status" .= statusCode (response ^. #responseStatusCode)
          , "body" .= TL.decodeUtf8 (response ^. #responseBody)
          ]
      throw e
    Left e -> throw e

fetchNewIndex :: JobsRunner ()
fetchNewIndex = localDomain "index-import" $ do
  logInfo_ "Fetching new index"
  System.runProcess_ "cabal update"
  System.runProcess_ "cp ~/.cabal/packages/hackage.haskell.org/01-index.tar 01-index/"
  System.runProcess_ "cd 01-index && tar -xf 01-index.tar"
  System.runProcess_ "make import-from-hackage"
  pool <- getPool
  liftIO $ void $ scheduleIndexImportJob pool

runner :: Job -> JobsRunner ()
runner job = localDomainEff' "job-runner" $
  case fromJSON (jobPayload job) of
    Error str -> logMessageEff' LogAttention "decode error" (toJSON str)
    Success val -> case val of
      MkReadme x -> makeReadme x
      FetchUploadTime x -> fetchUploadTime x
      ImportHackageIndex _ -> fetchNewIndex
