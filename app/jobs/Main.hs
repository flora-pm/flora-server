module Main where

import Arbiter.Core qualified as Arb
import Arbiter.Simple qualified as ArbS
import Arbiter.Worker qualified as Worker
import Control.Monad
import Data.Proxy
import Data.Text.Display
import Data.Text.IO qualified as T
import Data.Time
import Effectful
import Effectful.Concurrent (forkIO, runConcurrent)
import Effectful.Fail
import Effectful.FileSystem
import Effectful.Prometheus (runPrometheusMetrics)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setOnException
  , setPort
  )
import Network.Wai.Middleware.Prometheus qualified as WaiMetrics
import Prometheus qualified as P
import Prometheus.Metric.GHC qualified as P
import Effectful.Log qualified as Log
import Log
import RequireCallStack

import Flora.Environment
import Flora.Environment.Env
import Flora.Logging qualified as Logging
import Flora.Model.Job
import FloraJobs.Environment
import FloraJobs.Metrics
import FloraJobs.Runner qualified as Runner
import FloraJobs.Types
import FloraWeb.Common.Tracing

main :: IO ()
main = do
  jobsEnv <- runEff getFloraJobsEnv
  floraEnv <- runEff . runFailIO . runFileSystem $ getFloraEnv
  let baseURL = "http://localhost:" <> display jobsEnv.httpPort
  workerEnv <- ArbS.createSimpleEnv (Proxy @JobQueues) jobsEnv.connectionInfo "public"
  let withLogger = Logging.makeLogger floraEnv.mltp.logger
  runEff . runConcurrent $ do
    when floraEnv.mltp.prometheusEnabled $ do
      liftIO $ T.putStrLn $ "🔥 Exposing Prometheus metrics at " <> baseURL <> "/metrics"
      runPrometheusMetrics jobsEnv.metrics $ do
        void $ P.register P.ghcMetrics
        setGitHash
    withLogger $ \logger -> do
      void . forkIO $ runServer logger floraEnv jobsEnv
      defaultConfig <- liftIO $ Worker.defaultWorkerConfig jobsEnv.connectionInfo 50 (processJob workerEnv jobsEnv logger floraEnv)
      let config =
            defaultConfig
              { Worker.observabilityHooks =
                  Arb.defaultObservabilityHooks
                    { Arb.onJobFailure = \job message startTime endTime ->
                        liftIO $
                          runEff $
                            Log.runLog "flora-jobs" logger defaultLogLevel $
                              Log.logAttention message $
                                object
                                  [ "duration" .= diffUTCTime endTime startTime
                                  , "payload" .= job.payload
                                  ]
                    }
              }

      liftIO $ ArbS.runSimpleDb workerEnv $ Worker.runWorkerPool config

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
  -> Arb.JobHandler (ArbS.SimpleDb JobQueues IO) PackageJob ()
processJob workerEnv jobsRunnerEnv logger floraEnv _conn job =
  provideCallStack $
    liftIO $
      runJobRunner
        floraEnv.pool
        jobsRunnerEnv
        floraEnv
        logger
        (Log.localDomain "job-runner" $ Runner.runner workerEnv job)
