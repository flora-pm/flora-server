{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FloraJobs.Types where

import Commonmark qualified
import Control.Exception (Exception)
import Data.Aeson
import Data.Aeson.TH
import Data.Function ((&))
import Data.Pool hiding (PoolConfig)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Display
import Data.Text.Encoding.Error (UnicodeException)
import Data.Vector (Vector)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Types (QualifiedIdentifier)
import Deriving.Aeson
import Distribution.Pretty
import Distribution.Types.Version (Version)
import Distribution.Version (mkVersion, versionNumbers)
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
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Network.HTTP.Client
import OddJobs.Job (Job, LogEvent (..))
import OddJobs.Types (FailureMode)
import RequireCallStack
import Web.HttpApiData

import Distribution.Orphans.Version ()
import Flora.Environment.Env
import Flora.Import.Package.Types (ImportOutput)
import Flora.Import.Types (ImportError)
import Flora.Model.BlobStore.API
import Flora.Model.Package (PackageName (..))
import Flora.Model.Package.Types (Namespace)
import Flora.Model.Release.Types (ReleaseId (..))
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

type JobQueues =
  '[ '("package_jobs", PackageJob)
   ]

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

newtype IntAesonVersion = MkIntAesonVersion {unIntAesonVersion :: Version}
  deriving
    (Display, Pretty, ToHttpApiData)
    via Version

instance ToJSON IntAesonVersion where
  toJSON (MkIntAesonVersion x) = toJSON $ versionNumbers x

instance FromJSON IntAesonVersion where
  parseJSON val = MkIntAesonVersion . mkVersion <$> parseJSON val

data ReadmeJobPayload = ReadmeJobPayload
  { mpPackage :: PackageName
  , mpReleaseId :: ReleaseId -- needed to write the readme in db
  , mpVersion :: IntAesonVersion
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] ReadmeJobPayload)

data TarballJobPayload = TarballJobPayload
  { package :: PackageName
  , releaseId :: ReleaseId
  , version :: IntAesonVersion
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data UploadTimeJobPayload = UploadTimeJobPayload
  { packageName :: PackageName
  , releaseId :: ReleaseId
  , packageVersion :: IntAesonVersion
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] UploadTimeJobPayload)

data ChangelogJobPayload = ChangelogJobPayload
  { packageName :: PackageName
  , releaseId :: ReleaseId
  , packageVersion :: IntAesonVersion
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] ChangelogJobPayload)

data ImportHackageIndexPayload = ImportHackageIndexPayload
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] ImportHackageIndexPayload)

-- these represent the possible odd jobs we can run.
data PackageJob
  = FetchReadme ReadmeJobPayload
  | FetchTarball TarballJobPayload
  | FetchUploadTime UploadTimeJobPayload
  | FetchChangelog ChangelogJobPayload
  | ImportPackage ImportOutput
  | FetchPackageDeprecationList
  | FetchReleaseDeprecationList PackageName (Vector ReleaseId)
  | RefreshLatestVersions
  | RefreshIndex Text
  deriving stock (Generic)

-- TODO: Upstream these two ToJSON instances

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''PackageJob)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''FailureMode)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Job)

instance ToJSON LogEvent where
  toJSON = \case
    LogJobStart job -> toJSON ("start" :: Text, job)
    LogJobSuccess job time -> toJSON ("success" :: Text, job, time)
    LogJobFailed job exception failuremode finishTime ->
      toJSON ("failed" :: Text, show exception, job, failuremode, finishTime)
    LogJobTimeout job -> toJSON ("timed-out" :: Text, job)
    LogPoll -> toJSON ("poll" :: Text)
    LogWebUIRequest -> toJSON ("web-ui-request" :: Text)
    LogKillJobSuccess job -> toJSON ("kill-success" :: Text, job)
    LogKillJobFailed job -> toJSON ("kill-failed" :: Text, job)
    LogDeletionPoll data_ -> toJSON ("log-deletion-poll" :: Text, data_)
    LogText other -> toJSON ("other" :: Text, other)
