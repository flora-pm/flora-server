{-# OPTIONS_GHC -Wno-orphans #-}

module FloraJobs.Types where

import Commonmark qualified
import Control.Exception (Exception)
import Data.Aeson
import Data.Function ((&))
import Data.Pool hiding (PoolConfig)
import Data.Poolboy (poolboySettingsWith)
import Data.Text qualified as Text
import Data.Text.Encoding.Error (UnicodeException)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Types (QualifiedIdentifier)
import Distribution.Types.Version (Version)
import Effectful
import Effectful.FileSystem
import Effectful.Log hiding (LogLevel)
import Effectful.Log qualified as LogEff hiding (LogLevel)
import Effectful.Poolboy
import Effectful.PostgreSQL.Transact.Effect (DB, runDB)
import Effectful.Process.Typed
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Shared (State)
import Effectful.State.Static.Shared qualified as State
import Effectful.Time (Time, runTime)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Log hiding (LogLevel)
import Network.HTTP.Client
import OddJobs.ConfigBuilder
import OddJobs.Job (Config (..), Job, LogEvent (..), LogLevel (..))
import OddJobs.Types (ConcurrencyControl (..), UIConfig (..))

import Data.Set (Set)
import Flora.Environment
import Flora.Environment.Config
import Flora.Logging qualified as Logging
import Flora.Model.BlobStore.API
import Flora.Model.Job ()
import Flora.Model.Package.Types (Namespace, PackageName)

type JobsRunner =
  Eff
    '[ DB
     , Poolboy
     , Reader JobsRunnerEnv
     , BlobStoreAPI
     , Log
     , Time
     , TypedProcess
     , FileSystem
     , State (Set (Namespace, PackageName, Version))
     , IOE
     ]

runJobRunner :: Pool Connection -> JobsRunnerEnv -> FloraEnv -> Logger -> JobsRunner a -> IO a
runJobRunner pool runnerEnv floraEnv logger jobRunner =
  jobRunner
    & withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
    & runDB pool
    & runPoolboy (poolboySettingsWith floraEnv.dbConfig.connections)
    & runReader runnerEnv
    & ( case floraEnv.features.blobStoreImpl of
          Just (BlobStoreFS fp) -> runBlobStoreFS fp
          _ -> runBlobStorePure
      )
    & LogEff.runLog "flora-jobs" logger defaultLogLevel
    & runTime
    & runTypedProcess
    & runFileSystem
    & State.evalState mempty
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

makeConfig
  :: JobsRunnerEnv
  -> FloraEnv
  -> Logger
  -> Pool PG.Connection
  -> (Job -> JobsRunner ())
  -> Config
makeConfig runnerEnv floraEnv logger pool runnerContinuation =
  mkConfig
    (\level event -> structuredLogging floraEnv.config logger level event)
    jobTableName
    pool
    (MaxConcurrentJobs 100)
    (runJobRunner pool runnerEnv floraEnv logger . runnerContinuation)
    (\x -> x{cfgDefaultMaxAttempts = 3, cfgDefaultJobTimeout = 36000})

makeUIConfig :: FloraConfig -> Logger -> Pool PG.Connection -> UIConfig
makeUIConfig cfg logger pool =
  mkUIConfig (structuredLogging cfg logger) jobTableName pool id

structuredLogging :: FloraConfig -> Logger -> LogLevel -> LogEvent -> IO ()
structuredLogging FloraConfig{..} logger level event =
  runEff
    . withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
    . runTime
    . Logging.runLog environment logger
    $ localDomain "odd-jobs"
    $ case level of
      LevelDebug -> logMessage Log.LogTrace "LevelDebug" (toJSON event)
      LevelInfo -> logMessage Log.LogInfo "LevelInfo" (toJSON event)
      LevelWarn -> logMessage Log.LogAttention "LevelWarn" (toJSON event)
      LevelError -> logMessage Log.LogAttention "LevelError" (toJSON event)
      (LevelOther x) -> logMessage Log.LogAttention ("LevelOther " <> Text.pack (show x)) (toJSON event)
