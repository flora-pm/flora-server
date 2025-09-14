{-# OPTIONS_GHC -Wno-orphans #-}

module FloraJobs.Types where

import Commonmark qualified
import Control.Exception (Exception)
import Data.Function ((&))
import Data.Pool hiding (PoolConfig)
import Data.Set (Set)
import Data.Text.Encoding.Error (UnicodeException)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Types (QualifiedIdentifier)
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
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Network.HTTP.Client
import RequireCallStack

import Flora.Environment.Env
import Flora.Import.Types (ImportError)
import Flora.Model.BlobStore.API
import Flora.Model.Job ()
import Flora.Model.Package.Types (Namespace, PackageName)
import Flora.Monad

type JobsRunner a =
  FloraM
    '[ DB
     , Reader JobsRunnerEnv
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
    a

runJobRunner
  :: RequireCallStack
  => Pool Connection
  -> JobsRunnerEnv
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
    & LogEff.runLog "flora-jobs" logger defaultLogLevel
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

data OddJobException where
  DecodeFailed :: HasCallStack => UnicodeException -> OddJobException
  MarkdownFailed :: HasCallStack => Commonmark.ParseError -> OddJobException
  deriving (Exception)

instance Show OddJobException where
  show (DecodeFailed x) = renderExceptionWithCallstack x "DecodeFailed"
  show (MarkdownFailed x) = renderExceptionWithCallstack x "MarkdownFailed"

renderExceptionWithCallstack :: (HasCallStack, Show a) => a -> String -> String
renderExceptionWithCallstack errors valueConstructor =
  "("
    <> valueConstructor
    <> " $ "
    <> show errors
    <> "/*"
    <> prettyCallStack callStack
    <> " */)"

jobTableName :: QualifiedIdentifier
jobTableName = "oddjobs"

data JobsRunnerEnv = JobsRunnerEnv
  { httpManager :: Manager
  }
  deriving stock (Generic)
