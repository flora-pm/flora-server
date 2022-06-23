{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Flora.OddJobs.Types where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader ( ReaderT, MonadReader, runReaderT )
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Database.PostgreSQL.Simple.Types (QualifiedIdentifier)
import Distribution.Pretty
import Distribution.Version (Version, versionNumbers, mkVersion)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, prettyCallStack, callStack)
import Log hiding (LogLevel)
import Network.HTTP.Client
import OddJobs.Job (Job, LogEvent (..), LogLevel (..))
import OddJobs.Types (FailureMode)
import qualified Commonmark
import qualified Data.Text as Text

import Flora.Environment.Config
import FloraWeb.Server.Logging
import Flora.Model.Package (PackageName(..))
import Flora.Model.Release (ReleaseId)

newtype JobsRunnerM a = JobsRunnerM {getJobRunnerM :: ReaderT RunnerEnv (LogT IO) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadLog
    , MonadReader RunnerEnv
    )

runJobRunnerM :: RunnerEnv -> Logger -> JobsRunnerM a -> IO a
runJobRunnerM runnerEnv logger jobRunner = 
  Log.runLogT "flora-jobs" logger defaultLogLevel (runReaderT (getJobRunnerM jobRunner) runnerEnv)

data RunnerEnv = RunnerEnv
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
  deriving newtype (Pretty)

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

-- these represent the possible odd jobs we can run.
data FloraOddJobs
  = MkReadme ReadmePayload
  | DoNothing -- needed to keep this type tagged
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

jobTableName :: QualifiedIdentifier
jobTableName = "oddjobs"

-- proly should upstream these,
-- kinda dumb the "support" structured logging without the most
-- common method being used
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

structuredLogging :: FloraConfig -> Logger -> LogEvent -> LogLevel -> IO ()
structuredLogging FloraConfig{..} logger b =
  runLog environment logger . localDomain "odd-jobs" . \case
    LevelDebug -> logTrace "LevelDebug" b
    LevelInfo -> logInfo "LevelInfo" b
    LevelWarn -> logAttention "LevelWarn" b
    LevelError -> logAttention "LevelError" b
    (LevelOther x) -> logAttention ("LevelOther " <> Text.pack (show x)) b

