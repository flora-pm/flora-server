{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Flora.Model.Job where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Text.Display
import Distribution.Pretty
import Distribution.Version (Version, mkVersion, versionNumbers)
import OddJobs.Job (Job, LogEvent (..))
import OddJobs.Types (FailureMode)
import Servant (ToHttpApiData)

import Data.Vector (Vector)
import Deriving.Aeson
import Distribution.Orphans.Version ()
import Flora.Import.Package.Types (ImportOutput)
import Flora.Model.Package (PackageName (..))
import Flora.Model.Release.Types (ReleaseId (..))

newtype IntAesonVersion = MkIntAesonVersion {unIntAesonVersion :: Version}
  deriving
    (Pretty, ToHttpApiData, Display)
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
    (ToJSON, FromJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] ReadmeJobPayload)

data UploadTimeJobPayload = UploadTimeJobPayload
  { packageName :: PackageName
  , releaseId :: ReleaseId
  , packageVersion :: IntAesonVersion
  }
  deriving stock (Generic)
  deriving
    (ToJSON, FromJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] UploadTimeJobPayload)

data ChangelogJobPayload = ChangelogJobPayload
  { packageName :: PackageName
  , releaseId :: ReleaseId
  , packageVersion :: IntAesonVersion
  }
  deriving stock (Generic)
  deriving
    (ToJSON, FromJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] ChangelogJobPayload)

data ImportHackageIndexPayload = ImportHackageIndexPayload
  deriving stock (Generic)
  deriving
    (ToJSON, FromJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] ImportHackageIndexPayload)

-- these represent the possible odd jobs we can run.
data FloraOddJobs
  = FetchReadme ReadmeJobPayload
  | FetchUploadTime UploadTimeJobPayload
  | FetchChangelog ChangelogJobPayload
  | ImportHackageIndex ImportHackageIndexPayload
  | ImportPackage ImportOutput
  | FetchPackageDeprecationList
  | FetchReleaseDeprecationList PackageName (Vector ReleaseId)
  | RefreshLatestVersions
  deriving stock (Generic)

-- TODO: Upstream these two ToJSON instances

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''FloraOddJobs)
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
    LogText other -> toJSON ("other" :: Text, other)
