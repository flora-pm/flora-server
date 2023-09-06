{-# LANGUAGE QuasiQuotes #-}

-- | Represents the various jobs that can be run
module FloraJobs.Scheduler
  ( scheduleReadmeJob
  , scheduleChangelogJob
  , scheduleUploadTimeJob
  , scheduleIndexImportJob
  , schedulePackageDeprecationListJob
  , scheduleReleaseDeprecationListJob
  , scheduleRefreshLatestVersions
  , checkIfIndexImportJobIsNotRunning
  , jobTableName
  --   prefer using smart constructors.
  , ReadmeJobPayload (..)
  , FloraOddJobs (..)
  , IntAesonVersion (..)
  )
where

import Data.Pool
import Data.Time qualified as Time
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Distribution.Types.Version
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Time qualified as Time
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

scheduleChangelogJob :: Pool PG.Connection -> ReleaseId -> PackageName -> Version -> IO Job
scheduleChangelogJob pool rid package version =
  withResource
    pool
    ( \res ->
        createJob
          res
          jobTableName
          (FetchChangelog $ ChangelogJobPayload package rid $ MkIntAesonVersion version)
    )

scheduleUploadTimeJob :: Pool PG.Connection -> ReleaseId -> PackageName -> Version -> IO Job
scheduleUploadTimeJob pool releaseId packageName version =
  withResource
    pool
    ( \res ->
        createJob
          res
          jobTableName
          (FetchUploadTime $ UploadTimeJobPayload packageName releaseId (MkIntAesonVersion version))
    )

scheduleIndexImportJob :: Pool PG.Connection -> IO Job
scheduleIndexImportJob pool =
  withResource
    pool
    ( \conn -> do
        t <- Time.currentTime
        let runAt = Time.addUTCTime Time.nominalDay t
        scheduleJob
          conn
          jobTableName
          (ImportHackageIndex ImportHackageIndexPayload)
          runAt
    )

schedulePackageDeprecationListJob :: Pool PG.Connection -> IO Job
schedulePackageDeprecationListJob pool =
  withResource
    pool
    ( \conn ->
        createJob
          conn
          jobTableName
          FetchPackageDeprecationList
    )

scheduleReleaseDeprecationListJob :: Pool PG.Connection -> (PackageName, Vector ReleaseId) -> IO Job
scheduleReleaseDeprecationListJob pool (package, releaseIds) =
  withResource
    pool
    ( \conn ->
        createJob
          conn
          jobTableName
          (FetchReleaseDeprecationList package releaseIds)
    )

scheduleRefreshLatestVersions :: Pool PG.Connection -> IO Job
scheduleRefreshLatestVersions pool =
  withResource
    pool
    ( \conn ->
        createJob
          conn
          jobTableName
          RefreshLatestVersions
    )

checkIfIndexImportJobIsNotRunning :: JobsRunner Bool
checkIfIndexImportJobIsNotRunning = do
  Log.logInfo_ "Checking if the index import job is not running…"
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
      Log.logInfo_ "Index import job is running"
      pure True
    Just (Only 0) -> do
      Log.logInfo_ "Index import job is running"
      pure True
    Just (Only 1) -> do
      Log.logInfo_ "Index import job is running"
      pure True
    _ -> do
      Log.logInfo_ "Index import job not running"
      pure False
