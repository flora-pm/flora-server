-- | Represents the various jobs that can be run
module FloraJobs.Scheduler
  ( -- * Job payload constructors
    readmeJob
  , tarballJob
  , changelogJob
  , uploadInformationJob
  , packageDeprecationListJob
  , releaseDeprecationListJob
  , refreshLatestVersionsJob
  , packageMaintainersListJob
  , packageUploadersJob

    -- * Enqueuing
  , scheduleJobs
  , scheduleMissingMetadataJobs
  , schedulePackageDeprecationListJob
  , schedulePackageUploadersJob
  , scheduleRefreshIndex
  , checkIfIndexRefreshJobIsPlanned
  --   prefer using smart constructors.
  , ReadmeJobPayload (..)
  , IntAesonVersion (..)
  )
where

import Arbiter.Core qualified as Arb
import Arbiter.Simple qualified as ArbS
import Control.Monad
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Types.Version
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as Concurrent
import Effectful.Exception qualified as Exception
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Log

import Flora.Database
import Flora.Debug.ThreadDump (labelCurrentThread)
import Flora.Environment.Env
import Flora.Model.Job
import Flora.Model.Package.Query qualified as PackageQuery
import Flora.Model.Package.Types
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types
import Flora.Model.Release.Query qualified as ReleaseQuery
import Flora.Model.Release.Types
import Flora.Monad

--------------------------------------------------------------------------------
-- Job payload constructors
--------------------------------------------------------------------------------

readmeJob :: ReleaseId -> PackageName -> Version -> PackageJob
readmeJob rid package version =
  FetchReadme $ ReadmeJobPayload package rid $ MkIntAesonVersion version

tarballJob :: ReleaseId -> Namespace -> PackageName -> Version -> PackageJob
tarballJob rid namespace package version =
  FetchTarball $ TarballJobPayload namespace package rid $ MkIntAesonVersion version

changelogJob :: ReleaseId -> PackageName -> Version -> PackageJob
changelogJob rid package version =
  FetchChangelog $ ChangelogJobPayload package rid $ MkIntAesonVersion version

uploadInformationJob :: ReleaseId -> PackageName -> Version -> PackageJob
uploadInformationJob rid package version =
  FetchUploadInformation $ UploadInformationJobPayload package rid $ MkIntAesonVersion version

packageDeprecationListJob :: PackageJob
packageDeprecationListJob = FetchPackageDeprecationList

releaseDeprecationListJob :: (PackageName, Vector ReleaseId) -> PackageJob
releaseDeprecationListJob (package, releaseIds) =
  FetchReleaseDeprecationList package releaseIds

refreshLatestVersionsJob :: PackageJob
refreshLatestVersionsJob = RefreshLatestVersions

packageMaintainersListJob :: PackageName -> PackageJob
packageMaintainersListJob = FetchPackageMaintainers

packageUploadersJob :: PackageJob
packageUploadersJob = FetchPackageUploaders

jobBatchSize :: Int
jobBatchSize = 1000

scheduleJobs
  :: MonadUnliftIO m
  => ArbS.SimpleEnv JobQueues
  -> Vector PackageJob
  -> m Int64
scheduleJobs env = go 0
  where
    go !inserted jobs
      | Vector.null jobs = pure inserted
      | otherwise = do
          let (batch, rest) = Vector.splitAt jobBatchSize jobs
          batchInserted <-
            ArbS.runSimpleDb env $
              Arb.insertJobsBatch_ $
                fmap Arb.defaultJob (Vector.toList batch)
          go (inserted + batchInserted) rest

scheduleMissingMetadataJobs
  :: ( Concurrent :> es
     , IOE :> es
     , Log :> es
     , Reader FloraEnv :> es
     )
  => ArbS.SimpleEnv JobQueues
  -> Bool
  -- ^ Whether to enqueue tarball fetching jobs
  -> FloraM es ()
scheduleMissingMetadataJobs env withTarballs = do
  FloraEnv{pool} <- Reader.ask
  void $ Concurrent.forkIO $ do
    liftIO $ labelCurrentThread "schedule-metadata"

    schedulingPass "readme" $ do
      releases <- withReadOnlyPool pool ReleaseQuery.getHackagePackageReleasesWithoutReadme
      scheduleJobs env $
        fmap (\(releaseId, version, package) -> readmeJob releaseId package version) releases

    schedulingPass "upload-information" $ do
      releases <- withReadOnlyPool pool ReleaseQuery.getHackagePackageReleasesWithoutUploadInformation
      scheduleJobs env $
        fmap (\(releaseId, version, package) -> uploadInformationJob releaseId package version) releases

    schedulingPass "changelog" $ do
      releases <- withReadOnlyPool pool ReleaseQuery.getHackagePackageReleasesWithoutChangelog
      scheduleJobs env $
        fmap (\(releaseId, version, package) -> changelogJob releaseId package version) releases

    when withTarballs $
      schedulingPass "tarball" $ do
        releases <- withReadOnlyPool pool ReleaseQuery.getHackagePackageReleasesWithoutTarball
        scheduleJobs env $
          fmap (\(releaseId, version, package) -> tarballJob releaseId (Namespace "hackage") package version) releases

    schedulingPass "release-deprecation" $ do
      packages <- withReadOnlyPool pool ReleaseQuery.getHackagePackagesWithoutReleaseDeprecationInformation
      scheduleJobs env $ fmap releaseDeprecationListJob packages

    schedulingPass "refresh-latest-versions" $
      scheduleJobs env $
        Vector.singleton refreshLatestVersionsJob

    schedulingPass "maintainers" $ do
      packages <- withReadOnlyPool pool PackageQuery.getPackagesWithoutMaintainersInformation
      scheduleJobs env $ fmap (packageMaintainersListJob . snd) packages

schedulingPass
  :: Log :> es
  => Text
  -> Eff es Int64
  -> Eff es ()
schedulingPass name action =
  Exception.trySync action >>= \case
    Right count ->
      Log.logInfo "Scheduled metadata jobs" $
        object ["pass" .= name, "jobs" .= count]
    Left err ->
      Log.logAttention "Could not schedule metadata jobs" $
        object ["pass" .= name, "error" .= show err]

schedulePackageDeprecationListJob
  :: MonadUnliftIO m
  => ArbS.SimpleEnv JobQueues
  -> m Int64
schedulePackageDeprecationListJob env =
  scheduleJobs env $ Vector.singleton packageDeprecationListJob

schedulePackageUploadersJob
  :: MonadUnliftIO m
  => ArbS.SimpleEnv JobQueues
  -> m Int64
schedulePackageUploadersJob env =
  scheduleJobs env $ Vector.singleton packageUploadersJob

scheduleRefreshIndex :: ArbS.SimpleEnv JobQueues -> Text -> IO (Maybe (Arb.JobRead PackageJob))
scheduleRefreshIndex env indexName = ArbS.runSimpleDb env $ do
  now <- liftIO Time.getCurrentTime
  let scheduledTime = Time.addUTCTime Time.nominalDay now
  let arbJob = Arb.defaultJob $ RefreshIndex indexName
  Arb.insertJob arbJob{Arb.notVisibleUntil = Just scheduledTime, Arb.dedupKey = Just (Arb.IgnoreDuplicate ("index-refresh-" <> indexName))}

checkIfIndexRefreshJobIsPlanned
  :: ( IOE :> es
     , Log :> es
     , Reader FloraEnv :> es
     )
  => ArbS.SimpleEnv JobQueues
  -> FloraM es ()
checkIfIndexRefreshJobIsPlanned env = do
  FloraEnv{pool} <- Reader.ask
  Log.logInfo_ "Checking if the index refresh job is planned…"
  indexes <- withReadOnlyPool pool Query.listPackageIndexes
  forM_ indexes $ \index -> do
    Log.logInfo "Scheduling index refresh" $ object ["index" .= index.repository]
    void $ liftIO $ scheduleRefreshIndex env index.repository
