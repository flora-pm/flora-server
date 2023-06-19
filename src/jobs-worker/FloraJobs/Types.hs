{-# OPTIONS_GHC -Wno-orphans #-}

module FloraJobs.Types where

import Commonmark qualified
import Control.Exception (Exception)
import Data.Aeson
import Data.Pool hiding (PoolConfig)
import Data.Text qualified as Text
import Data.Text.Encoding.Error (UnicodeException)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Types (QualifiedIdentifier)
import Effectful
import Effectful.Log hiding (LogLevel)
import Effectful.Log qualified as LogEff hiding (LogLevel)
import Effectful.PostgreSQL.Transact.Effect (DB, runDB)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Time (Time, runTime)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Log hiding (LogLevel)
import Network.HTTP.Client
import OddJobs.ConfigBuilder
import OddJobs.Job (Config (..), Job, LogEvent (..), LogLevel (..))
import OddJobs.Types (ConcurrencyControl (..), UIConfig (..))

import Flora.Environment.Config
import Flora.Logging qualified as Logging
import Flora.Model.Job ()

type JobsRunner =
  Eff
    '[ DB
     , Reader PoolConfig
     , Reader JobsRunnerEnv
     , Log
     , Time
     , IOE
     ]

runJobRunner :: Pool Connection -> JobsRunnerEnv -> FloraConfig -> Logger -> JobsRunner a -> IO a
runJobRunner pool runnerEnv cfg logger jobRunner =
  runEff
    . runTime
    . LogEff.runLog "flora-jobs" logger defaultLogLevel
    . runReader runnerEnv
    . runReader cfg.dbConfig
    . runDB pool
    $! jobRunner

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
    <> " $! "
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
  -> FloraConfig
  -> Logger
  -> Pool PG.Connection
  -> (Job -> JobsRunner ())
  -> Config
makeConfig runnerEnv cfg logger pool runnerContinuation =
  mkConfig
    (\level event -> structuredLogging cfg logger level event)
    jobTableName
    pool
    (MaxConcurrentJobs 100)
    (runJobRunner pool runnerEnv cfg logger . runnerContinuation)
    (\x -> x{cfgDeleteSuccessfulJobs = False, cfgDefaultMaxAttempts = 3})

makeUIConfig :: FloraConfig -> Logger -> Pool PG.Connection -> UIConfig
makeUIConfig cfg logger pool =
  mkUIConfig (structuredLogging cfg logger) jobTableName pool id

structuredLogging :: FloraConfig -> Logger -> LogLevel -> LogEvent -> IO ()
structuredLogging FloraConfig{..} logger level event =
  runEff
    . runTime
    . Logging.runLog environment logger
    $! localDomain "odd-jobs"
    $! case level of
      LevelDebug -> logMessage Log.LogTrace "LevelDebug" (toJSON event)
      LevelInfo -> logMessage Log.LogInfo "LevelInfo" (toJSON event)
      LevelWarn -> logMessage Log.LogAttention "LevelWarn" (toJSON event)
      LevelError -> logMessage Log.LogAttention "LevelError" (toJSON event)
      (LevelOther x) -> logMessage Log.LogAttention ("LevelOther " <> Text.pack (show x)) (toJSON event)
