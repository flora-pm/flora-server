module FloraJobs.Types where

import Data.Function ((&))
import Data.Set (Set)
import Data.Text.Display (display)
import Distribution.Types.Version (Version)
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
import Effectful.State.Static.Shared (State)
import Effectful.State.Static.Shared qualified as State
import Effectful.Time (Time, runTime)
import Effectful.Trace (Trace)
import Effectful.Trace qualified as Trace
import GHC.Stack (prettyCallStack)
import Monitor.Tracing.Zipkin (Zipkin (..))
import RequireCallStack

import Distribution.Orphans.Version ()
import Flora.Environment.Env
import Flora.Import.Types (ImportError)
import Flora.Model.BlobStore.API
import Flora.Model.Package.Types (Namespace, PackageName (..))
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
     , State (Set (Namespace, PackageName, Version))
     , Trace
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
  runTrace <-
    if floraEnv.environment == Production
      then do
        zipkin <- liftIO $ Tracing.newZipkin floraEnv.mltp.zipkinHost "flora-jobs"
        pure $ Trace.runTrace zipkin.zipkinTracer
      else pure Trace.runNoTrace
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
    & State.evalState mempty
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
