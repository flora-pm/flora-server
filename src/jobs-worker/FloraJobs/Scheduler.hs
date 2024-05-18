{-# LANGUAGE QuasiQuotes #-}

-- | Represents the various jobs that can be run
module FloraJobs.Scheduler
  ( scheduleReadmeJob
  , scheduleTarballJob
  , scheduleChangelogJob
  , scheduleUploadTimeJob
  , schedulePackageDeprecationListJob
  , scheduleReleaseDeprecationListJob
  , scheduleRefreshLatestVersions
  , scheduleRefreshIndexes
  , checkIfIndexRefreshJobIsPlanned
  , jobTableName
  --   prefer using smart constructors.
  , ReadmeJobPayload (..)
  , FloraOddJobs (..)
  , IntAesonVersion (..)
  )
where

import Data.Aeson (ToJSON)
import Data.Pool
import Data.Time qualified as Time
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Distribution.Types.Version
import Effectful
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect
import Log
import OddJobs.Job (Job (..), createJob, scheduleJob)

import Flora.Model.Job
import Flora.Model.Package
import Flora.Model.Release.Types
import FloraJobs.Types

scheduleReadmeJob :: Pool PG.Connection -> ReleaseId -> PackageName -> Version -> IO Job
scheduleReadmeJob pool rid package version =
  withResource
    pool
    ( \res ->
        createJob
          res
          jobTableName
          (FetchReadme $ ReadmeJobPayload package rid $ MkIntAesonVersion version)
    )

scheduleTarballJob :: Pool PG.Connection -> ReleaseId -> PackageName -> Version -> IO Job
scheduleTarballJob pool rid package version =
  createJobWithResource pool $ FetchTarball $ TarballJobPayload package rid $ MkIntAesonVersion version

scheduleChangelogJob :: Pool PG.Connection -> ReleaseId -> PackageName -> Version -> IO Job
scheduleChangelogJob pool rid package version =
  createJobWithResource pool $ FetchChangelog $ ChangelogJobPayload package rid $ MkIntAesonVersion version

scheduleUploadTimeJob :: Pool PG.Connection -> ReleaseId -> PackageName -> Version -> IO Job
scheduleUploadTimeJob pool releaseId packageName version =
  createJobWithResource pool $
    FetchUploadTime $
      UploadTimeJobPayload packageName releaseId (MkIntAesonVersion version)

schedulePackageDeprecationListJob :: Pool PG.Connection -> IO Job
schedulePackageDeprecationListJob pool =
  createJobWithResource pool FetchPackageDeprecationList

scheduleReleaseDeprecationListJob
  :: Pool PG.Connection -> (PackageName, Vector ReleaseId) -> IO Job
scheduleReleaseDeprecationListJob pool (package, releaseIds) =
  createJobWithResource pool (FetchReleaseDeprecationList package releaseIds)

scheduleRefreshLatestVersions :: Pool PG.Connection -> IO Job
scheduleRefreshLatestVersions pool = createJobWithResource pool RefreshLatestVersions

scheduleRefreshIndexes :: Pool PG.Connection -> IO Job
scheduleRefreshIndexes pool = withResource pool $ \conn -> do
  now <- Time.getCurrentTime
  scheduleJob conn jobTableName RefreshIndexes (Time.addUTCTime Time.nominalDay now)

createJobWithResource :: ToJSON p => Pool PG.Connection -> p -> IO Job
createJobWithResource pool job =
  withResource pool $ \conn -> createJob conn jobTableName job

checkIfIndexRefreshJobIsPlanned
  :: ( DB :> es
     , Log :> es
     )
  => Eff es Bool
checkIfIndexRefreshJobIsPlanned = do
  Log.logInfo_ "Checking if the index refresh job is planned…"
  (result :: Maybe (Only Int)) <-
    dbtToEff $
      queryOne_
        Select
        [sql|
              select count(*)
              from "oddjobs"
              where payload ->> 'tag' = 'RefreshIndexes'
              and status = 'queued'
      |]
  case result of
    Just (Only 1) -> do
      Log.logInfo_ "Index refresh job is planned"
      pure True
    _ -> do
      Log.logInfo_ "Index refresh job not not planned"
      pure False
