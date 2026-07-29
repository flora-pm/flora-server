module Main where

import Arbiter.Core qualified as Arb
import Arbiter.Simple qualified as ArbS
import Arbiter.Worker qualified as Worker
import Control.Monad
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as T
import Data.Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Concurrent (forkIO, runConcurrent)
import Effectful.Fail
import Effectful.FileSystem
import Effectful.Log
import Effectful.Log qualified as Log
import Effectful.Prometheus (runPrometheusMetrics)
import Log
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setOnException
  , setPort
  )
import Network.Wai.Middleware.Prometheus qualified as WaiMetrics
import NoThunks.Class
import Options.Applicative (execParser)
import Prometheus qualified as P
import Prometheus.Metric.GHC qualified as P
import Prometheus.Metric.Proc qualified as P
import RequireCallStack
import System.Exit (exitFailure)
import System.Info qualified as System

import Flora.Database (withReadOnlyPool)
import Flora.Debug.ThreadDump (installThreadDumpHandler, labelCurrentThread)
import Flora.Environment
import Flora.Environment.Config (ConnectionInfo (..), FloraConfig (..))
import Flora.Environment.Env
import Flora.Logging (makeLogger)
import Flora.Model.Job
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types (PackageIndex (..))
import Flora.Tracing qualified as Tracing
import FloraJobs.Environment
import FloraJobs.Metrics
import FloraJobs.QueueMetrics qualified as QueueMetrics
import FloraJobs.Runner qualified as Runner
import FloraJobs.Types
import FloraWeb.Common.Tracing

main :: IO ()
main = do
  floraConfig <- execParser parseConfig
  jobsEnv <- runEff . runFailIO $ getFloraJobsEnv floraConfig
  floraEnv <- runEff . runFailIO . runFileSystem $ getFloraEnv floraConfig
  let baseURL = "http://localhost:" <> display jobsEnv.httpPort
  let workerEnv = ArbS.createSimpleEnvWithPool (Proxy @JobQueues) jobsEnv.pool "public"
  let withLogger = makeLogger "logs/flora-jobs.json" floraEnv.mltp.logger
  traceRunner <- Tracing.newTraceRunner floraEnv.mltp.zipkinHost "flora-jobs"
  runEff . runConcurrent $ do
    liftIO $ startEventlogSocket floraEnv.mltp.eventlogSocketDirectory
    liftIO installThreadDumpHandler
    when floraEnv.mltp.prometheusEnabled $ do
      liftIO $ T.putStrLn $ "🔥 Exposing Prometheus metrics at " <> baseURL <> "/metrics"
      runPrometheusMetrics jobsEnv.metrics $ do
        void $ P.register P.ghcMetrics
        when (System.os == "linux") $ void $ P.register P.procMetrics
        setGitHash
    withLogger $ \logger -> do
      runLog "flora-server" logger Log.LogTrace $
        checkJobsEnvForThunks jobsEnv
      when floraEnv.mltp.prometheusEnabled $ do
        void . forkIO $ do
          liftIO $ labelCurrentThread "jobs-http-server"
          runServer logger floraEnv jobsEnv
        void . forkIO $ do
          liftIO $ labelCurrentThread "jobs-queue-metrics"
          runLog ("flora-jobs-" <> display floraEnv.environment) logger defaultLogLevel $
            QueueMetrics.runQueueMetricsLoop workerEnv jobsEnv.metrics
      indexRefreshCrons <-
        runLog ("flora-jobs-" <> display floraEnv.environment) logger defaultLogLevel $
          provideCallStack $ do
            indexes <- withReadOnlyPool jobsEnv.pool Query.listPackageIndexes
            indexRefreshCronJobs indexes
      defaultConfig <- liftIO $
        Worker.defaultBatchedWorkerConfig (connString floraEnv.config.connectionInfo) 50 1 $
          \(job :| _) callbacks -> do
            processJob workerEnv jobsEnv logger floraEnv traceRunner job
            Worker.ack callbacks job
      let instrumentedHooks =
            if floraEnv.mltp.prometheusEnabled
              then metricsObservabilityHooks jobsEnv.metrics
              else Arb.defaultObservabilityHooks
      let config =
            defaultConfig
              { Worker.cronJobs = indexRefreshCrons
              , Worker.observabilityHooks =
                  instrumentedHooks
                    { Arb.onJobFailure = \job message startTime endTime -> do
                        let duration = diffUTCTime endTime startTime
                        Arb.onJobFailure instrumentedHooks job message startTime endTime
                        liftIO $
                          runEff $
                            Log.runLog ("flora-jobs-" <> display floraEnv.environment) logger defaultLogLevel $
                              Log.logAttention message $
                                object
                                  [ "duration" .= duration
                                  , "payload" .= job.payload
                                  ]
                    }
              }

      liftIO $ ArbS.runSimpleDb workerEnv $ Worker.runWorkerPool config
  where
    connString connectionInfo =
      Text.encodeUtf8 $
        "host="
          <> connectionInfo.connectHost
          <> " port="
          <> Text.pack (show connectionInfo.connectPort)
          <> " user="
          <> connectionInfo.connectUser
          <> " password="
          <> connectionInfo.connectPassword
          <> " dbname="
          <> connectionInfo.connectDatabase

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

runServer
  :: IOE :> es
  => Log.Logger
  -> FloraEnv
  -> FloraJobsEnv
  -> Eff es ()
runServer logger floraEnv jobsEnv = do
  let warpSettings =
        setPort (fromIntegral jobsEnv.httpPort) $
          setOnException
            ( handleExceptions
                "flora-jobs"
                logger
                floraEnv.environment
                floraEnv.mltp
            )
            defaultSettings

  liftIO $ runSettings warpSettings WaiMetrics.metricsApp

processJob
  :: ArbS.SimpleEnv JobQueues
  -> FloraJobsEnv
  -> Log.Logger
  -> FloraEnv
  -> Tracing.TraceRunner
  -> Arb.JobRead PackageJob
  -> ArbS.SimpleDb JobQueues IO ()
processJob workerEnv jobsRunnerEnv logger floraEnv traceRunner job =
  provideCallStack $
    liftIO $
      runJobRunner
        jobsRunnerEnv
        floraEnv
        logger
        traceRunner
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
