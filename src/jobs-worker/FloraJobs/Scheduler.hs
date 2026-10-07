-- | Represents the various jobs that can be run
module FloraJobs.Scheduler
  ( -- * Job payload constructors
    readmeJob
  , tarballJob
  , changelogJob
  , uploadInformationJob

    -- * Enqueuing
  , scheduleJobs
  , scheduleMissingMetadataJobs
  , metadataPasses
  , runMetadataPass
  , schedulePackageDeprecationListJob
  , schedulePackageUploadersJob
  --   prefer using smart constructors.
  , ReadmeJobPayload (..)
  , IntAesonVersion (..)
  )
where

import Arbiter.Core qualified as Arb
import Arbiter.Simple qualified as ArbS
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Simple qualified as PG
import Distribution.Types.Version
import Effectful
import Effectful.Log (Log)
import Log

import Flora.Database
import Flora.Model.Job
import Flora.Model.Package.Query qualified as PackageQuery
import Flora.Model.Package.Types
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

releaseDeprecationListJob :: (PackageName, Vector ReleaseId) -> PackageJob
releaseDeprecationListJob (package, releaseIds) =
  FetchReleaseDeprecationList package releaseIds

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
    RefreshDependents -> "refresh-dependents"
    FetchPackageUploaders -> "package-uploaders"
    PruneFeedEntries -> "prune-feed-entries"
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
     )
  => Pool PG.Connection
  -> ArbS.SimpleEnv JobQueues
  -> MetadataPass
  -> FloraM es ()
runMetadataPass pool env pass = do
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
      scheduleJobs env $ Vector.singleton RefreshLatestVersions
    RefreshDependentsPass ->
      scheduleJobs env $ Vector.singleton RefreshDependents
    MaintainersPass -> do
      packages <- withReadOnlyPool pool PackageQuery.getPackagesWithoutMaintainersInformation
      scheduleJobs env $ fmap (FetchPackageMaintainers . snd) packages
  Log.logInfo "Scheduled metadata jobs" $
    object ["pass" .= passName pass, "jobs" .= count]

schedulePackageDeprecationListJob
  :: MonadUnliftIO m
  => ArbS.SimpleEnv JobQueues
  -> m Int64
schedulePackageDeprecationListJob env =
  scheduleJobs env $ Vector.singleton FetchPackageDeprecationList

schedulePackageUploadersJob
  :: MonadUnliftIO m
  => ArbS.SimpleEnv JobQueues
  -> m Int64
schedulePackageUploadersJob env =
  scheduleJobs env $ Vector.singleton FetchPackageUploaders
