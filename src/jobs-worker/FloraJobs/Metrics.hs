{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module FloraJobs.Metrics
  ( JobsRunnerMetrics (..)
  , registerMetrics
  , setGitHash
  , metricsObservabilityHooks
  , recordJobFailure
  , publishQueueStats
  ) where

import Arbiter.Core qualified as Arb
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Effectful
import Effectful.Prometheus
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import Prometheus
import Prometheus qualified as P

import Flora.Model.Job (PackageJob, jobTypeLabel)
import Paths_flora (version)

data JobsRunnerMetrics = JobsRunnerMetrics
  { buildInformation :: P.Vector P.Label2 P.Gauge
  , jobsClaimed :: P.Vector P.Label1 P.Counter
  , jobsTotal :: P.Vector P.Label2 P.Counter
  , jobDuration :: P.Vector P.Label1 P.Histogram
  , jobQueueLatency :: P.Vector P.Label1 P.Histogram
  , jobRetries :: P.Vector P.Label1 P.Counter
  , jobsDeadLettered :: P.Vector P.Label1 P.Counter
  , queueDepth :: P.Vector P.Label1 P.Gauge
  , queueOldestSeconds :: P.Gauge
  , dlqSize :: P.Gauge
  , queueLastRefresh :: P.Gauge
  }

-- Opaque, long-lived Prometheus metric state held for the jobs process lifetime:
-- assert WHNF only, do not descend into the library's intentionally-lazy metric map.
deriving via
  OnlyCheckWhnfNamed "FloraJobs.Metrics.JobsRunnerMetrics" JobsRunnerMetrics
  instance
    NoThunks JobsRunnerMetrics

jobDurationBuckets :: [Double]
jobDurationBuckets = P.exponentialBuckets 0.05 2 19

queueLatencyBuckets :: [Double]
queueLatencyBuckets = P.exponentialBuckets 0.1 2 16

registerMetrics :: IOE :> es => Eff es JobsRunnerMetrics
registerMetrics = do
  buildInformation <-
    P.register $
      P.vector ("git_revision", "version") $
        P.gauge
          P.Info
            { metricName = "build_information"
            , metricHelp = "Build information"
            }
  jobsClaimed <-
    P.register $
      P.vector "job_type" $
        P.counter
          P.Info
            { metricName = "flora_jobs_claimed_total"
            , metricHelp =
                "Jobs picked up by a worker. Subtracting flora_jobs_total from this \
                \gives the jobs that vanished without a terminal outcome (lease stolen, \
                \row gone, nacked, or the runner died mid-job)"
            }
  jobsTotal <-
    P.register $
      P.vector ("job_type", "outcome") $
        P.counter
          P.Info
            { metricName = "flora_jobs_total"
            , metricHelp =
                "Job attempts that reached a terminal outcome, by kind and outcome. \
                \A job retried three times and then dead-lettered contributes four \
                \failures, not one"
            }
  jobDuration <-
    P.register $
      P.vector "job_type" $
        P.histogram
          P.Info
            { metricName = "flora_job_duration_seconds"
            , metricHelp = "Time a job handler spent running, by kind"
            }
          jobDurationBuckets
  jobQueueLatency <-
    P.register $
      P.vector "job_type" $
        P.histogram
          P.Info
            { metricName = "flora_job_queue_latency_seconds"
            , metricHelp = "Time a job waited between being enqueued and being claimed"
            }
          queueLatencyBuckets
  jobRetries <-
    P.register $
      P.vector "job_type" $
        P.counter
          P.Info
            { metricName = "flora_job_retries_total"
            , metricHelp = "Jobs rescheduled after a retryable failure"
            }
  jobsDeadLettered <-
    P.register $
      P.vector "job_type" $
        P.counter
          P.Info
            { metricName = "flora_jobs_dead_lettered_total"
            , metricHelp =
                "Jobs that exhausted their attempts and moved to the DLQ while a worker \
                \held them. Arbiter's reaper also sweeps exhausted jobs to the DLQ \
                \without a hook, so flora_job_dlq_size can grow faster than this"
            }
  queueDepth <-
    P.register $
      P.vector "state" $
        P.gauge
          P.Info
            { metricName = "flora_job_queue_depth"
            , metricHelp = "Jobs in the queue table, by state"
            }
  queueOldestSeconds <-
    P.register $
      P.gauge
        P.Info
          { metricName = "flora_job_queue_oldest_seconds"
          , metricHelp = "Age of the oldest runnable job in the queue"
          }
  dlqSize <-
    P.register $
      P.gauge
        P.Info
          { metricName = "flora_job_dlq_size"
          , metricHelp = "Jobs sitting in the dead-letter queue"
          }
  queueLastRefresh <-
    P.register $
      P.gauge
        P.Info
          { metricName = "flora_job_queue_last_refresh_timestamp_seconds"
          , metricHelp =
              "Unix time of the last successful queue count. The queue gauges are a \
              \snapshot, so without this a dead poller is indistinguishable from a \
              \calm queue"
          }
  pure JobsRunnerMetrics{..}

setGitHash
  :: Metrics JobsRunnerMetrics :> es
  => Eff es ()
setGitHash =
  setLabelledGauge (.buildInformation) ($(gitHash), T.pack (showVersion version)) 1.0

metricsObservabilityHooks
  :: MonadIO m
  => JobsRunnerMetrics
  -> Arb.ObservabilityHooks m PackageJob
metricsObservabilityHooks metrics =
  Arb.defaultObservabilityHooks
    { Arb.onJobClaimed = \job claimTime ->
        recordJobClaimed metrics job claimTime
    , Arb.onJobSuccess = \job startTime endTime ->
        recordJobSuccess metrics job (diffUTCTime endTime startTime)
    , Arb.onJobFailure = \job _message startTime endTime ->
        recordJobFailure metrics job (diffUTCTime endTime startTime)
    , Arb.onJobRetry = \job _backoffDelay ->
        recordJobRetry metrics job
    , Arb.onJobFailedAndMovedToDLQ = \_message job ->
        recordJobDeadLettered metrics job
    }

queueWait :: UTCTime -> Maybe UTCTime -> UTCTime -> NominalDiffTime
queueWait insertedAt notVisibleUntil claimTime =
  max 0 (diffUTCTime claimTime becameClaimableAt)
  where
    becameClaimableAt = maybe insertedAt (max insertedAt) notVisibleUntil

recordJobClaimed :: MonadIO m => JobsRunnerMetrics -> Arb.JobRead PackageJob -> UTCTime -> m ()
recordJobClaimed metrics job claimTime = liftIO $ do
  let jobType = jobTypeLabel job.payload
  P.withLabel metrics.jobsClaimed jobType P.incCounter
  P.withLabel
    metrics.jobQueueLatency
    jobType
    (`P.observe` realToFrac (queueWait job.insertedAt job.notVisibleUntil claimTime))

recordJobSuccess :: MonadIO m => JobsRunnerMetrics -> Arb.JobRead PackageJob -> NominalDiffTime -> m ()
recordJobSuccess metrics job duration = liftIO $ do
  let jobType = jobTypeLabel job.payload
  P.withLabel metrics.jobDuration jobType (`P.observe` realToFrac duration)
  P.withLabel metrics.jobsTotal (jobType, "success") P.incCounter

recordJobFailure :: MonadIO m => JobsRunnerMetrics -> Arb.JobRead PackageJob -> NominalDiffTime -> m ()
recordJobFailure metrics job duration = liftIO $ do
  let jobType = jobTypeLabel job.payload
  P.withLabel metrics.jobDuration jobType (`P.observe` realToFrac duration)
  P.withLabel metrics.jobsTotal (jobType, "failure") P.incCounter

recordJobRetry :: MonadIO m => JobsRunnerMetrics -> Arb.JobRead PackageJob -> m ()
recordJobRetry metrics job =
  liftIO $ P.withLabel metrics.jobRetries (jobTypeLabel job.payload) P.incCounter

recordJobDeadLettered :: MonadIO m => JobsRunnerMetrics -> Arb.JobRead PackageJob -> m ()
recordJobDeadLettered metrics job =
  liftIO $ P.withLabel metrics.jobsDeadLettered (jobTypeLabel job.payload) P.incCounter

publishQueueStats :: MonadIO m => JobsRunnerMetrics -> Arb.QueueStats -> Int64 -> m ()
publishQueueStats metrics stats deadLettered = liftIO $ do
  for_ perState $ \(state, value) ->
    P.withLabel metrics.queueDepth state (`P.setGauge` fromIntegral value)
  P.setGauge metrics.queueOldestSeconds (fromMaybe 0 stats.oldestReadyAgeSeconds)
  P.setGauge metrics.dlqSize (fromIntegral deadLettered)
  now <- getPOSIXTime
  P.setGauge metrics.queueLastRefresh (realToFrac now)
  where
    perState =
      [ ("ready", stats.readyJobs)
      , ("scheduled", stats.scheduledJobs)
      , ("backoff", stats.backoffJobs)
      , ("throttled", stats.throttledJobs)
      , ("suspended", stats.suspendedJobs)
      , ("in_flight", stats.inFlightJobs)
      ]
