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
  , metadataPasses
  , runMetadataPass
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
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Types.Version
import Effectful
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Log

import Flora.Database
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

jobDedupKey :: PackageJob -> Arb.DedupKey
jobDedupKey =
  Arb.IgnoreDuplicate . \case
    FetchReadme payload -> "readme-" <> display payload.mpReleaseId
    FetchTarball payload -> "tarball-" <> display payload.releaseId
    FetchUploadInformation payload -> "upload-information-" <> display payload.releaseId
    FetchChangelog payload -> "changelog-" <> display payload.releaseId
    FetchPackageMaintainers package -> "maintainers-" <> display package
    FetchReleaseDeprecationList package _ -> "release-deprecation-" <> display package
    FetchPackageDeprecationList -> "package-deprecation-list"
    RefreshLatestVersions -> "refresh-latest-versions"
    FetchPackageUploaders -> "package-uploaders"
    RefreshIndex indexName -> "index-refresh-" <> indexName
    ScheduleMetadata pass -> "metadata-pass-" <> passName pass

toJobWrite :: PackageJob -> Arb.JobWrite PackageJob
toJobWrite job = (Arb.defaultJob job){Arb.dedupKey = Just (jobDedupKey job)}

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
                fmap toJobWrite (Vector.toList batch)
          go (inserted + batchInserted) rest

metadataPasses
  :: Bool
  -- ^ Whether to enqueue the tarball pass
  -> [MetadataPass]
metadataPasses withTarballs = filter enabled [minBound .. maxBound]
  where
    enabled TarballPass = withTarballs
    enabled _ = True

passName :: MetadataPass -> Text
passName = Text.replace "_" "-" . metadataPassLabel

scheduleMissingMetadataJobs
  :: MonadUnliftIO m
  => ArbS.SimpleEnv JobQueues
  -> Bool
  -> m Int64
scheduleMissingMetadataJobs env withTarballs =
  scheduleJobs env $
    Vector.fromList $
      fmap ScheduleMetadata (metadataPasses withTarballs)

runMetadataPass
  :: ( IOE :> es
     , Log :> es
     , Reader FloraEnv :> es
     )
  => ArbS.SimpleEnv JobQueues
  -> MetadataPass
  -> FloraM es ()
runMetadataPass env pass = do
  FloraEnv{pool} <- Reader.ask
  count <- case pass of
    ReadmePass -> do
      releases <- withReadOnlyPool pool ReleaseQuery.getHackagePackageReleasesWithoutReadme
      scheduleJobs env $
        fmap (\(releaseId, version, package) -> readmeJob releaseId package version) releases
    UploadInformationPass -> do
      releases <- withReadOnlyPool pool ReleaseQuery.getHackagePackageReleasesWithoutUploadInformation
      scheduleJobs env $
        fmap (\(releaseId, version, package) -> uploadInformationJob releaseId package version) releases
    ChangelogPass -> do
      releases <- withReadOnlyPool pool ReleaseQuery.getHackagePackageReleasesWithoutChangelog
      scheduleJobs env $
        fmap (\(releaseId, version, package) -> changelogJob releaseId package version) releases
    TarballPass -> do
      releases <- withReadOnlyPool pool ReleaseQuery.getHackagePackageReleasesWithoutTarball
      scheduleJobs env $
        fmap (\(releaseId, version, package) -> tarballJob releaseId (Namespace "hackage") package version) releases
    ReleaseDeprecationPass -> do
      packages <- withReadOnlyPool pool ReleaseQuery.getHackagePackagesWithoutReleaseDeprecationInformation
      scheduleJobs env $ fmap releaseDeprecationListJob packages
    RefreshLatestVersionsPass ->
      scheduleJobs env $ Vector.singleton refreshLatestVersionsJob
    MaintainersPass -> do
      packages <- withReadOnlyPool pool PackageQuery.getPackagesWithoutMaintainersInformation
      scheduleJobs env $ fmap (packageMaintainersListJob . snd) packages
  Log.logInfo "Scheduled metadata jobs" $
    object ["pass" .= passName pass, "jobs" .= count]

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
  let arbJob = toJobWrite $ RefreshIndex indexName
  Arb.insertJob arbJob{Arb.notVisibleUntil = Just scheduledTime}

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
