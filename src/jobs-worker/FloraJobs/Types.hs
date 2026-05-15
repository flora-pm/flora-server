module FloraJobs.Types where

import Data.Function ((&))
import Data.Pool hiding (PoolConfig)
import Data.Set (Set)
import Data.Text.Display (display)
import Database.PostgreSQL.Simple (Connection)
import Distribution.Types.Version (Version)
import Effectful
import Effectful.Concurrent.Async
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem
import Effectful.Log hiding (LogLevel)
import Effectful.Log qualified as LogEff hiding (LogLevel)
import Effectful.PostgreSQL.Transact.Effect (DB, runDB)
import Effectful.Process.Typed
import Effectful.Prometheus
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Shared (State)
import Effectful.State.Static.Shared qualified as State
import Effectful.Time (Time, runTime)
import GHC.Stack (prettyCallStack)
import RequireCallStack

import Distribution.Orphans.Version ()
import Flora.Environment.Env
import Flora.Import.Types (ImportError)
import Flora.Model.BlobStore.API
import Flora.Model.Package (PackageName (..))
import Flora.Model.Package.Types (Namespace)
import FloraJobs.Environment

type JobsRunner =
  Eff
    '[ DB
     , Reader FloraJobsEnv
     , BlobStoreAPI
     , Log
     , Time
     , TypedProcess
     , FileSystem
     , State (Set (Namespace, PackageName, Version))
     , Reader FloraEnv
     , Concurrent
     , Metrics AppMetrics
     , Error ImportError
     , IOE
     ]

runJobRunner
  :: RequireCallStack
  => Pool Connection
  -> FloraJobsEnv
  -> FloraEnv
  -> Logger
  -> JobsRunner a
  -> IO a
runJobRunner pool runnerEnv floraEnv logger jobRunner =
  jobRunner
    & withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
    & runDB pool
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
    & Reader.runReader floraEnv
    & runConcurrent
    & runPrometheusMetrics floraEnv.metrics
    & Error.runErrorWith
      ( \callstack err -> do
          liftIO $ putStrLn $ prettyCallStack callstack
          pure $ error $ show err
      )
    & runEff
