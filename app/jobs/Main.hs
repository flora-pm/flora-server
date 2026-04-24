module Main where

import Arbiter.Core qualified as Arb
import Arbiter.Servant qualified as Servant
import Arbiter.Servant.UI qualified as ArbUI
import Arbiter.Simple qualified as ArbS
import Arbiter.Worker qualified as Worker
import Data.Proxy
import Effectful
import Effectful.Concurrent
import Effectful.Fail
import Effectful.FileSystem
import Log qualified
import Network.Wai.Handler.Warp
import RequireCallStack

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
  workerEnv <- ArbS.createSimpleEnv (Proxy @JobQueues) jobsEnv.connectionInfo "public"
  let withLogger = Logging.makeLogger floraEnv.mltp.logger
  runEff $ do
    withLogger $ \logger -> do
      config <- liftIO $ Worker.defaultWorkerConfig jobsEnv.connectionInfo 50 (processJob workerEnv jobsEnv logger floraEnv)
      liftIO $ ArbS.runSimpleDb workerEnv $ Worker.runWorkerPool config

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
