module Flora.SchedulerSpec (spec) where

import Control.Monad (void)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Database.PostgreSQL.Simple (Only (..))
import Effectful
import Effectful.Exception (finally)
import Effectful.Reader.Static (ask)
import RequireCallStack

import Flora.Database
import Flora.Environment.Env (FloraEnv (..))
import Flora.Model.Job
import Flora.Model.Package.Types (PackageName (..))
import Flora.TestUtils
import FloraJobs.Scheduler

spec :: RequireCallStack => TestEff TestTree
spec =
  testTheseInOrder
    "scheduler"
    [ testThis "scheduleJobs inserts every job across batch boundaries" testScheduleJobsSpansBatches
    , testThis "scheduleJobs does not enqueue a job twice" testJobsAreDeduplicated
    , testThis "scheduleMissingMetadataJobs honours the tarball gate" testTarballGate
    , testThis "scheduleMissingMetadataJobs is idempotent" testPassesAreDeduplicated
    ]

testScheduleJobsSpansBatches :: RequireCallStack => TestEff ()
testScheduleJobsSpansBatches = withoutMarkedJobs $ do
  FloraEnv{pool, workerEnv} <- ask
  let jobCount = 2500
  let jobs =
        Vector.generate jobCount $ \i ->
          FetchPackageMaintainers $ PackageName $ jobMarker <> Text.pack (show i)
  inserted <- scheduleJobs workerEnv jobs
  assertEqual
    "scheduleJobs reports every job as inserted"
    (fromIntegral jobCount :: Int64)
    inserted
  enqueued <- withReadOnlyPool pool countMarkedJobs
  assertEqual "every job reached the queue" jobCount enqueued

-- | Covers both routes to a duplicate: the same key twice inside one batch,
-- and a pass re-run while its previous jobs are still queued.
testJobsAreDeduplicated :: RequireCallStack => TestEff ()
testJobsAreDeduplicated = withoutMarkedJobs $ do
  FloraEnv{pool, workerEnv} <- ask
  let jobs = Vector.singleton $ FetchPackageMaintainers (PackageName $ jobMarker <> "duplicated")
  collapsed <- scheduleJobs workerEnv (jobs <> jobs)
  assertEqual "two copies in one batch collapse into one" (1 :: Int64) collapsed
  again <- scheduleJobs workerEnv jobs
  assertEqual "enqueuing it while it is still pending is a no-op" (0 :: Int64) again
  enqueued <- withReadOnlyPool pool countMarkedJobs
  assertEqual "only one row reached the queue" 1 enqueued

testTarballGate :: RequireCallStack => TestEff ()
testTarballGate = withoutPassJobs $ do
  FloraEnv{pool, workerEnv} <- ask
  assertEqual
    "leaving the tarball pass out drops exactly one pass"
    (length (metadataPasses True) - 1)
    (length (metadataPasses False))
  inserted <- scheduleMissingMetadataJobs workerEnv False
  assertEqual
    "every pass but the tarball one is enqueued"
    (fromIntegral (length (metadataPasses False)) :: Int64)
    inserted
  tarballs <- withReadOnlyPool pool countTarballPassJobs
  assertEqual "no tarball pass reached the queue" 0 tarballs

testPassesAreDeduplicated :: RequireCallStack => TestEff ()
testPassesAreDeduplicated = withoutPassJobs $ do
  FloraEnv{pool, workerEnv} <- ask
  inserted <- scheduleMissingMetadataJobs workerEnv True
  assertEqual
    "the first call enqueues every pass"
    (fromIntegral (length (metadataPasses True)) :: Int64)
    inserted
  _ <- scheduleMissingMetadataJobs workerEnv True
  enqueued <- withReadOnlyPool pool countPassJobs
  assertEqual
    "the second call does not stack a second sweep"
    (length (metadataPasses True))
    enqueued

jobMarker :: Text
jobMarker = "scheduler-spec-package-"

countMarkedJobs :: ReadDB :> es => Eff es Int
countMarkedJobs =
  queryCount "select count(*) from package_jobs where payload::text like ?" (Only marker)
  where
    marker = "%" <> jobMarker <> "%"

countPassJobs :: ReadDB :> es => Eff es Int
countPassJobs =
  queryCount_ "select count(*) from package_jobs where payload->>'tag' = 'ScheduleMetadata'"

countTarballPassJobs :: ReadDB :> es => Eff es Int
countTarballPassJobs =
  queryCount_
    "select count(*) from package_jobs \
    \where payload->>'tag' = 'ScheduleMetadata' and payload->>'contents' = 'TarballPass'"

withoutMarkedJobs :: RequireCallStack => TestEff a -> TestEff a
withoutMarkedJobs action = action `finally` deleteMarkedJobs

withoutPassJobs :: RequireCallStack => TestEff a -> TestEff a
withoutPassJobs action = action `finally` deletePassJobs

deleteMarkedJobs :: RequireCallStack => TestEff ()
deleteMarkedJobs = do
  FloraEnv{pool} <- ask
  withReadWritePool pool $
    void $
      execute "delete from package_jobs where payload::text like ?" (Only marker)
  where
    marker = "%" <> jobMarker <> "%"

deletePassJobs :: RequireCallStack => TestEff ()
deletePassJobs = do
  FloraEnv{pool} <- ask
  withReadWritePool pool $
    void $
      execute_ "delete from package_jobs where payload->>'tag' = 'ScheduleMetadata'"
