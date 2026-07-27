module Flora.SchedulerSpec where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Database.PostgreSQL.Simple (Only (..))
import Effectful
import Effectful.Reader.Static (ask)
import RequireCallStack

import Flora.Database
import Flora.Environment.Env (FloraEnv (..))
import Flora.Model.Package.Types (PackageName (..))
import Flora.TestUtils
import FloraJobs.Scheduler (packageMaintainersListJob, scheduleJobs)

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "scheduler"
    [ testThis "scheduleJobs inserts every job across batch boundaries" testScheduleJobsSpansBatches
    ]

jobCount :: Int
jobCount = 2500

testScheduleJobsSpansBatches :: RequireCallStack => TestEff ()
testScheduleJobsSpansBatches = do
  FloraEnv{pool, workerEnv} <- ask
  let jobs =
        Vector.generate jobCount $ \i ->
          packageMaintainersListJob $ PackageName $ jobMarker <> Text.pack (show i)
  inserted <- scheduleJobs workerEnv jobs
  assertEqual
    "scheduleJobs reports every job as inserted"
    (fromIntegral jobCount :: Int64)
    inserted
  enqueued <- withReadOnlyPool pool countMarkedJobs
  assertEqual "every job reached the queue" jobCount enqueued

jobMarker :: Text
jobMarker = "scheduler-spec-package-"

countMarkedJobs :: ReadDB :> es => Eff es Int
countMarkedJobs =
  queryCount "select count(*) from package_jobs where payload::text like ?" (Only marker)
  where
    marker = "%" <> jobMarker <> "%"
