module Main where

import Arbiter.Core qualified as Arb
import Arbiter.Simple qualified as ArbS
import Arbiter.Worker qualified as Worker
import Data.Proxy
import Data.Text.IO qualified as T
import Control.Monad
import FloraWeb.Common.Tracing
import Effectful.Prometheus (runPrometheusMetrics)
import Data.Text.Display
import Effectful
import Effectful.Fail
import Effectful.FileSystem
import Network.Wai.Middleware.Prometheus qualified as WaiMetrics
import Log qualified
import Prometheus qualified as P
import Prometheus.Metric.GHC qualified as P
import RequireCallStack
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setOnException
  , setPort
  )

import Flora.Environment
import Flora.Environment.Env
import Flora.Environment.Jobs
import Flora.Logging qualified as Logging
import Flora.Model.Job
import FloraJobs.Runner qualified as Runner
import FloraJobs.Types

main :: IO ()
main = do
  jobsEnv <- runEff getFloraJobsEnv
  floraEnv <- runEff . runFailIO . runFileSystem $ getFloraEnv
  let baseURL = "http://localhost:" <> display jobsEnv.httpPort
  workerEnv <- ArbS.createSimpleEnv (Proxy @JobQueues) jobsEnv.connectionInfo "public"
  let withLogger = Logging.makeLogger floraEnv.mltp.logger
  runEff $ do
    when floraEnv.mltp.prometheusEnabled $ do
      liftIO $ T.putStrLn $ "🔥 Exposing Prometheus metrics at " <> baseURL <> "/metrics"
      runPrometheusMetrics floraEnv.metrics $ do
        void $ P.register P.ghcMetrics
    withLogger $ \logger -> do
      config <- liftIO $ Worker.defaultWorkerConfig jobsEnv.connectionInfo 50 (processJob workerEnv jobsEnv logger floraEnv)
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
  liftIO
    $ runSettings warpSettings WaiMetrics.metricsApp

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
