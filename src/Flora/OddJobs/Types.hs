{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Flora.OddJobs.Types where

import qualified Commonmark
import Control.Exception (Exception)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Display
import Data.Text.Encoding.Error (UnicodeException)
import Database.PostgreSQL.Simple.Types (QualifiedIdentifier)
import Distribution.Pretty
import Distribution.Version (Version, mkVersion, versionNumbers)
import Effectful
import Effectful.Log
import qualified Effectful.Log as LogEff
import Effectful.Reader.Static (Reader, runReader)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Log hiding (LogLevel (..))
import qualified Log
import Network.HTTP.Client
import OddJobs.Job (Job, LogEvent (..), LogLevel (..))
import OddJobs.Types (FailureMode)
import Servant (ToHttpApiData)

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Effectful.PostgreSQL.Transact.Effect (DB, runDB)
import Effectful.Time (Time, runCurrentTimeIO)
import Flora.Environment.Config
import Flora.Model.Package (PackageName (..))
import Flora.Model.Release.Types (ReleaseId (..))
import qualified FloraWeb.Server.Logging as Logging

type JobsRunner =
  Eff
    '[ DB
     , Reader JobsRunnerEnv
     , Logging
     , Time
     , IOE
     ]

runJobRunner :: Pool Connection -> JobsRunnerEnv -> Logger -> JobsRunner a -> IO a
runJobRunner pool runnerEnv logger jobRunner =
  runEff
    . runCurrentTimeIO
    . LogEff.runLogging "flora-jobs" logger defaultLogLevel
    . runReader runnerEnv
    . runDB pool
    $ jobRunner

data JobsRunnerEnv = JobsRunnerEnv
  { httpManager :: Manager
  }
  deriving stock (Generic)

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

newtype IntAesonVersion = MkIntAesonVersion {unIntAesonVersion :: Version}
  deriving
    (Pretty, ToHttpApiData, Display)
    via Version

instance ToJSON IntAesonVersion where
  toJSON (MkIntAesonVersion x) = toJSON $ versionNumbers x

instance FromJSON IntAesonVersion where
  parseJSON val = MkIntAesonVersion . mkVersion <$> parseJSON val

data ReadmePayload = MkReadmePayload
  { mpPackage :: PackageName
  , mpReleaseId :: ReleaseId -- needed to write the readme in db
  , mpVersion :: IntAesonVersion
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data FetchUploadTimePayload = FetchUploadTimePayload
  { packageName :: PackageName
  , releaseId :: ReleaseId
  , packageVersion :: IntAesonVersion
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data ImportHackageIndexPayload = ImportHackageIndexPayload
  { tag :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

-- these represent the possible odd jobs we can run.
data FloraOddJobs
  = MkReadme ReadmePayload
  | FetchUploadTime FetchUploadTimePayload
  | ImportHackageIndex ImportHackageIndexPayload
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

jobTableName :: QualifiedIdentifier
jobTableName = "oddjobs"

-- TODO: Upstream these two ToJSON instances
deriving instance ToJSON FailureMode
deriving instance ToJSON Job

instance ToJSON LogEvent where
  toJSON = \case
    LogJobStart job -> toJSON ("start" :: Text, job)
    LogJobSuccess job time -> toJSON ("success" :: Text, job, time)
    LogJobFailed job exception failuremode finishTime -> toJSON ("failed" :: Text, show exception, job, failuremode, finishTime)
    LogJobTimeout job -> toJSON ("timed-out" :: Text, job)
    LogPoll -> toJSON ("poll" :: Text)
    LogWebUIRequest -> toJSON ("web-ui-request" :: Text)
    LogText other -> toJSON ("other" :: Text, other)

structuredLogging :: FloraConfig -> Logger -> LogLevel -> LogEvent -> IO ()
structuredLogging FloraConfig{..} logger level event =
  runEff
    . runCurrentTimeIO
    . Logging.runLog environment logger
    $ localDomainEff' "odd-jobs"
    $ case level of
      LevelDebug -> logMessageEff' Log.LogTrace "LevelDebug" (toJSON event)
      LevelInfo -> logMessageEff' Log.LogInfo "LevelInfo" (toJSON event)
      LevelWarn -> logMessageEff' Log.LogAttention "LevelWarn" (toJSON event)
      LevelError -> logMessageEff' Log.LogAttention "LevelError" (toJSON event)
      (LevelOther x) -> logMessageEff' Log.LogAttention ("LevelOther " <> Text.pack (show x)) (toJSON event)
