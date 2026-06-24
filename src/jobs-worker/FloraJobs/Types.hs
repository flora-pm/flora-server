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
import Effectful.Tracing (Tracer)
import GHC.Stack (prettyCallStack)
import RequireCallStack

import Flora.Environment.Env
import Flora.Import.Types (ImportError)
import Flora.Model.BlobStore.API
import Flora.Tracing qualified as Tracing
import FloraJobs.Environment

type JobsRunner =
  Eff
    '[ Reader FloraJobsEnv
     , BlobStoreAPI
     , Log
     , Time
     , TypedProcess
     , FileSystem
     , Tracer
     , Reader FloraEnv
     , Concurrent
     , Metrics AppMetrics
     , Error ImportError
     , IOE
     ]

runJobRunner
  :: RequireCallStack
  => FloraJobsEnv
  -> FloraEnv
  -> Logger
  -> JobsRunner a
  -> IO a
runJobRunner runnerEnv floraEnv logger jobRunner = do
  runTrace <- do
    traceRunner <- liftIO $ Tracing.newTraceRunner floraEnv.mltp.zipkinHost "flora-jobs"
    pure $ Tracing.runTraceRunner traceRunner
  jobRunner
    & withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
    & Reader.runReader runnerEnv
    & ( case floraEnv.features.blobStoreImpl of
          Just (BlobStoreFS fp) -> runBlobStoreFS fp
          _ -> runBlobStorePure
      )
    & LogEff.runLog ("flora-jobs-" <> display floraEnv.environment) logger defaultLogLevel
    & runTime
    & runTypedProcess
    & runFileSystem
    & runTrace
    & Reader.runReader floraEnv
    & runConcurrent
    & runPrometheusMetrics floraEnv.metrics
    & Error.runErrorWith
      ( \callstack err -> do
          liftIO $ putStrLn $ prettyCallStack callstack
          throwIO $ userError (show err)
      )
    & runEff
