-- | Represents the various jobs that can be run
module FloraJobs.Scheduler
  ( scheduleReadmeJob
  , scheduleTarballJob
  , scheduleChangelogJob
  , scheduleUploadTimeJob
  , schedulePackageDeprecationListJob
  , scheduleReleaseDeprecationListJob
  , scheduleRefreshLatestVersions
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
import Data.Text (Text)
import Data.Time qualified as Time
import Data.Vector (Vector)
import Distribution.Types.Version
import Effectful
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect
import Log

import Flora.Model.Job
import Flora.Model.Package
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types
import Flora.Model.Release.Types

scheduleReadmeJob
  :: MonadUnliftIO m
  => ArbS.SimpleEnv JobQueues
  -> ReleaseId
  -> PackageName
  -> Version
  -> m (Maybe (Arb.JobRead PackageJob))
scheduleReadmeJob env rid package version =
  createJobWithResource env (FetchReadme $ ReadmeJobPayload package rid $ MkIntAesonVersion version)

scheduleTarballJob
  :: MonadUnliftIO m
  => ArbS.SimpleEnv JobQueues
  -> ReleaseId
  -> PackageName
  -> Version
  -> m (Maybe (Arb.JobRead PackageJob))
scheduleTarballJob env rid package version =
  createJobWithResource env $ FetchTarball $ TarballJobPayload package rid $ MkIntAesonVersion version

scheduleChangelogJob
  :: MonadUnliftIO m
  => ArbS.SimpleEnv JobQueues
  -> ReleaseId
  -> PackageName
  -> Version
  -> m (Maybe (Arb.JobRead PackageJob))
scheduleChangelogJob env rid package version =
  createJobWithResource env $ FetchChangelog $ ChangelogJobPayload package rid $ MkIntAesonVersion version

scheduleUploadTimeJob
  :: MonadUnliftIO m
  => ArbS.SimpleEnv JobQueues
  -> ReleaseId
  -> PackageName
  -> Version
  -> m (Maybe (Arb.JobRead PackageJob))
scheduleUploadTimeJob env releaseId packageName version =
  createJobWithResource env $
    FetchUploadTime $
      UploadTimeJobPayload packageName releaseId (MkIntAesonVersion version)

schedulePackageDeprecationListJob
  :: MonadUnliftIO m
  => ArbS.SimpleEnv JobQueues
  -> m (Maybe (Arb.JobRead PackageJob))
schedulePackageDeprecationListJob env =
  createJobWithResource env FetchPackageDeprecationList

scheduleReleaseDeprecationListJob
  :: MonadUnliftIO m => ArbS.SimpleEnv JobQueues -> (PackageName, Vector ReleaseId) -> m (Maybe (Arb.JobRead PackageJob))
scheduleReleaseDeprecationListJob env (package, releaseIds) =
  createJobWithResource env (FetchReleaseDeprecationList package releaseIds)

scheduleRefreshLatestVersions :: MonadUnliftIO m => ArbS.SimpleEnv JobQueues -> m (Maybe (Arb.JobRead PackageJob))
scheduleRefreshLatestVersions env = createJobWithResource env RefreshLatestVersions

scheduleRefreshIndex :: ArbS.SimpleEnv JobQueues -> Text -> IO (Maybe (Arb.JobRead PackageJob))
scheduleRefreshIndex env indexName = ArbS.runSimpleDb env $ do
  now <- liftIO Time.getCurrentTime
  let scheduledTime = Time.addUTCTime Time.nominalDay now
  let arbJob = Arb.defaultJob $ RefreshIndex indexName
  Arb.insertJob arbJob{Arb.notVisibleUntil = Just scheduledTime, Arb.dedupKey = Just (Arb.IgnoreDuplicate ("index-refresh-" <> indexName))}

createJobWithResource
  :: MonadUnliftIO m
  => ArbS.SimpleEnv JobQueues
  -> PackageJob
  -> m (Maybe (Arb.JobRead PackageJob))
createJobWithResource env job =
  ArbS.runSimpleDb env $
    Arb.insertJob (Arb.defaultJob job)

checkIfIndexRefreshJobIsPlanned
  :: ( DB :> es
     , IOE :> es
     , Log :> es
     )
  => ArbS.SimpleEnv JobQueues
  -> Eff es ()
checkIfIndexRefreshJobIsPlanned env = do
  Log.logInfo_ "Checking if the index refresh job is planned…"
  indexes <- Query.listPackageIndexes
  forM_ indexes $ \index -> do
    Log.logInfo "Scheduling index refresh" $ object ["index" .= index.repository]
    void $ liftIO $ scheduleRefreshIndex env index.repository
