module Main where

import Arbiter.Core qualified as Arb
import Arbiter.Core.Job.Types qualified as Job
import Arbiter.Simple qualified as ArbS
import Arbiter.Worker qualified as Worker
import Control.Monad
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.IO qualified as T
import Data.Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Concurrent (forkIO, runConcurrent)
import Effectful.Fail
import Effectful.Log
import Effectful.Log qualified as Log
import Effectful.Prometheus (runPrometheusMetrics)
import Log
import Network.HTTP.Types (status404)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setOnException
  , setPort
  )
import Network.Wai.Middleware.Heartbeat (heartbeatMiddleware)
import Network.Wai.Middleware.Prometheus qualified as WaiMetrics
import NoThunks.Class
import Options.Applicative (execParser)
import RequireCallStack
import System.Exit (exitFailure)

import Flora.Database (withReadOnlyPool)
import Flora.Debug.ThreadDump (installThreadDumpHandler, labelCurrentThread)
import Flora.Environment (parseConfig)
import Flora.Environment.Config (jobWorkerLimit)
import Flora.Logging (makeLogger)
import Flora.Model.Job
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types (PackageIndex (..))
import Flora.Tracing
import FloraJobs.Config (JobsMLTP (..))
import FloraJobs.Environment
import FloraJobs.Metrics
import FloraJobs.QueueMetrics qualified as QueueMetrics
import FloraJobs.Runner qualified as Runner
import FloraJobs.Types

main :: IO ()
main = do
  labelCurrentThread "flora-jobs-runner-main"
  floraConfig <- execParser (parseConfig "jobs_runner.kdl")
  jobsEnv <- runEff . runFailIO $ getFloraJobsEnv floraConfig
  let baseURL = "http://localhost:" <> display jobsEnv.httpPort
  workerEnv <- ArbS.createSimpleEnvWithPool (Proxy @JobQueues) jobsEnv.arbiterPool "public"
  let withLogger = makeLogger "logs/flora-jobs.json" jobsEnv.mltp.logger
  runEff . runConcurrent $ do
    liftIO installThreadDumpHandler
    when jobsEnv.mltp.prometheusEnabled $ do
      liftIO $ T.putStrLn $ "🔥 Exposing Prometheus metrics at " <> baseURL <> "/metrics"
      registerRuntimeMetrics
      runPrometheusMetrics jobsEnv.metrics setGitHash
    withLogger $ \logger -> do
      runLog "flora-server" logger Log.LogTrace $
        checkJobsEnvForThunks jobsEnv
      liftIO $ T.putStrLn $ "💓 Exposing heartbeat at " <> baseURL <> "/heartbeat"
      void . forkIO $ do
        liftIO $ labelCurrentThread "jobs-http-server"
        runServer logger jobsEnv
      when jobsEnv.mltp.prometheusEnabled $ do
        void . forkIO $ do
          liftIO $ labelCurrentThread "jobs-queue-metrics"
          runLog ("flora-jobs-" <> display jobsEnv.environment) logger defaultLogLevel $
            QueueMetrics.runQueueMetricsLoop workerEnv jobsEnv.metrics
      crons <-
        runLog ("flora-jobs-" <> display jobsEnv.environment) logger defaultLogLevel $
          provideCallStack $ do
            indexes <- withReadOnlyPool jobsEnv.pool Query.listPackageIndexes
            indexRefreshCrons <- indexRefreshCronJobs indexes
            feedRetentionCron <- feedRetentionCronJob
            pure (indexRefreshCrons <> feedRetentionCron)
      defaultConfig <- liftIO $
        Worker.batchedWorkerConfig (jobWorkerLimit jobsEnv.dbConfig) 1 $
          \(job :| _) callbacks -> do
            liftIO $ labelCurrentThread (Text.unpack ("job-" <> Job.queueName job <> "-" <> jobTypeLabel (Arb.payload job)))
            processJob workerEnv jobsEnv logger job
            Worker.ack callbacks job
      let instrumentedHooks =
            if jobsEnv.mltp.prometheusEnabled
              then metricsObservabilityHooks jobsEnv.metrics
              else Arb.defaultObservabilityHooks
      let config =
            defaultConfig
              { Worker.cronJobs = crons
              , Worker.observabilityHooks =
                  instrumentedHooks
                    { Arb.onJobFailure = \job message startTime endTime -> do
                        let duration = diffUTCTime endTime startTime
                        Arb.onJobFailure instrumentedHooks job message startTime endTime
                        liftIO $
                          runEff $
                            Log.runLog ("flora-jobs-" <> display jobsEnv.environment) logger defaultLogLevel $
                              Log.logAttention message $
                                object
                                  [ "duration" .= duration
                                  , "payload" .= Arb.payload job
                                  ]
                    }
              }

      liftIO $ ArbS.runSimpleDb workerEnv $ Worker.runWorkerPool config

indexRefreshCronJobs
  :: (IOE :> es, Log :> es)
  => Vector PackageIndex
  -> Eff es [Worker.CronJob PackageJob]
indexRefreshCronJobs indexes = do
  let (invalid, crons) = partitionEithers $ Vector.toList $ Vector.imap mkCronJob indexes
  forM_ invalid $ \(repository, parseError) ->
    Log.logAttention "Invalid cron expression for the index refresh" $
      object
        [ "index" .= repository
        , "error" .= parseError
        ]
  unless (null invalid) $ liftIO exitFailure
  forM_ crons $ \cron ->
    Log.logInfo "Scheduling index refresh" $
      object
        [ "schedule" .= cron.name
        , "cron_expression" .= cron.cronExpression
        ]
  pure crons
  where
    mkCronJob position index =
      case Worker.cronJob
        ("refresh-index-" <> index.repository)
        (everyTwelveHoursAt position)
        Worker.SkipOverlap
        (\_tickKind _tickTime -> Arb.defaultJob (RefreshIndex index.repository)) of
        Left parseError -> Left (index.repository, Text.pack parseError)
        Right cron -> Right cron
    everyTwelveHoursAt :: Int -> Text
    everyTwelveHoursAt position =
      let hour = (3 + position) `mod` 12
       in "0 " <> display hour <> "," <> display (hour + 12) <> " * * *"

feedRetentionCronJob
  :: (IOE :> es, Log :> es)
  => Eff es [Worker.CronJob PackageJob]
feedRetentionCronJob =
  case Worker.cronJob
    "prune-feed-entries"
    "30 2 * * *"
    Worker.SkipOverlap
    (\_tickKind _tickTime -> Arb.defaultJob PruneFeedEntries) of
    Left parseError -> do
      Log.logAttention "Invalid cron expression for the feed entry pruning" $
        object ["error" .= Text.pack parseError]
      liftIO exitFailure
    Right cron -> do
      Log.logInfo "Scheduling feed entry pruning" $
        object
          [ "schedule" .= cron.name
          , "cron_expression" .= cron.cronExpression
          ]
      pure [cron]

runServer
  :: IOE :> es
  => Log.Logger
  -> FloraJobsEnv
  -> Eff es ()
runServer logger jobsEnv = do
  let warpSettings =
        setPort (fromIntegral jobsEnv.httpPort) $
          setOnException
            ( handleExceptions
                "flora-jobs"
                logger
                jobsEnv.environment
                jobsEnv.mltp.sentryDSN
            )
            defaultSettings

  let app =
        if jobsEnv.mltp.prometheusEnabled
          then WaiMetrics.metricsApp
          else \_request respond -> respond $ responseLBS status404 [] "Not Found"
  liftIO $ runSettings warpSettings $ heartbeatMiddleware app

processJob
  :: ArbS.SimpleEnv JobQueues
  -> FloraJobsEnv
  -> Log.Logger
  -> Arb.JobRead PackageJob
  -> ArbS.SimpleDb JobQueues IO ()
processJob workerEnv jobsRunnerEnv logger job =
  provideCallStack $
    liftIO $
      runJobRunner
        jobsRunnerEnv
        logger
        (Log.localDomain "job-runner" $ Runner.runner workerEnv job)

checkJobsEnvForThunks :: (IOE :> es, Log :> es) => FloraJobsEnv -> Eff es ()
checkJobsEnvForThunks env = do
  mThunk <- liftIO $ noThunks [] env
  forM_ mThunk $ \info ->
    Log.logAttention
      "Unexpected thunk detected in JobsEnv (possible space leak): "
      $ object
        [ "thunk_context" .= info.thunkContext
        , "thunk_info" .= info.thunkInfo
        ]
