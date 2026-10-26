module FloraJobs.Types where

import Data.Function ((&))
import Data.Text.Display (display)
import Effectful
import Effectful.Concurrent.Async
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Exception
import Effectful.FileSystem
import Effectful.Log hiding (LogLevel)
import Effectful.Log qualified as LogEff hiding (LogLevel)
import Effectful.Process.Typed
import Effectful.Prometheus
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time (Time, runTime)
import GHC.Stack (prettyCallStack)
import RequireCallStack

import Flora.Domain.Import.Types (ImportError)
import Flora.Environment.Domain (DomainEnv)
import Flora.Model.BlobStore.API
import Flora.Monitoring (ImportMetrics)
import FloraJobs.Environment
import FloraJobs.Metrics (JobsMetrics (..))

type JobsRunner =
  Eff
    '[ Reader FloraJobsEnv
     , Reader DomainEnv
     , BlobStoreAPI
     , Log
     , Time
     , TypedProcess
     , FileSystem
     , Concurrent
     , Metrics ImportMetrics
     , Error ImportError
     , IOE
     ]

runJobRunner
  :: RequireCallStack
  => FloraJobsEnv
  -> Logger
  -> JobsRunner a
  -> IO a
runJobRunner runnerEnv logger jobRunner = do
  jobRunner
    & withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
    & Reader.runReader runnerEnv
    & Reader.runReader runnerEnv.domainEnv
    & withBlobStore runnerEnv.features
    & LogEff.runLog ("flora-jobs-" <> display runnerEnv.environment) logger defaultLogLevel
    & runTime
    & runTypedProcess
    & runFileSystem
    & runConcurrent
    & runPrometheusMetrics runnerEnv.metrics.importMetrics
    & Error.runErrorWith
      ( \callstack err -> do
          liftIO $ putStrLn $ prettyCallStack callstack
          throwIO $ userError (show err)
      )
    & runEff
