{-# LANGUAGE TemplateHaskell #-}

module Flora.Model.Job where

import Data.Aeson
import Data.Aeson.TH
import Data.Text
import Data.Text.Display
import Data.Vector (Vector)
import Deriving.Aeson
import Distribution.Pretty
import Distribution.Types.Version (Version)
import Distribution.Version (mkVersion, versionNumbers)
import Web.HttpApiData

import Flora.Model.Package.Types
import Flora.Model.Release.Types

type JobQueues =
  '[ '("package_jobs", PackageJob)
   ]

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
  { namespace :: Namespace
  , packageName :: PackageName
  , releaseId :: ReleaseId
  , packageVersion :: IntAesonVersion
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data UploadInformationJobPayload = UploadInformationJobPayload
  { packageName :: PackageName
  , releaseId :: ReleaseId
  , packageVersion :: IntAesonVersion
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] UploadInformationJobPayload)

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

data MetadataPass
  = ReadmePass
  | UploadInformationPass
  | ChangelogPass
  | TarballPass
  | ReleaseDeprecationPass
  | RefreshLatestVersionsPass
  | MaintainersPass
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

data PackageJob
  = FetchReadme ReadmeJobPayload
  | FetchTarball TarballJobPayload
  | FetchUploadInformation UploadInformationJobPayload
  | FetchChangelog ChangelogJobPayload
  | FetchPackageDeprecationList
  | FetchReleaseDeprecationList PackageName (Vector ReleaseId)
  | RefreshLatestVersions
  | RefreshIndex Text
  | FetchPackageMaintainers PackageName
  | FetchPackageUploaders
  | PruneFeedEntries
  | ScheduleMetadata MetadataPass
  deriving stock (Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''PackageJob)

jobTypeLabel :: PackageJob -> Text
jobTypeLabel = \case
  FetchReadme{} -> "fetch_readme"
  FetchTarball{} -> "fetch_tarball"
  FetchUploadInformation{} -> "fetch_upload_information"
  FetchChangelog{} -> "fetch_changelog"
  FetchPackageDeprecationList -> "fetch_package_deprecation_list"
  FetchReleaseDeprecationList{} -> "fetch_release_deprecation_list"
  RefreshLatestVersions -> "refresh_latest_versions"
  RefreshIndex{} -> "refresh_index"
  FetchPackageMaintainers{} -> "fetch_package_maintainers"
  FetchPackageUploaders -> "fetch_package_uploaders"
  PruneFeedEntries -> "prune_feed_entries"
  ScheduleMetadata pass -> "schedule_metadata_" <> metadataPassLabel pass

metadataPassLabel :: MetadataPass -> Text
metadataPassLabel = \case
  ReadmePass -> "readme"
  UploadInformationPass -> "upload_information"
  ChangelogPass -> "changelog"
  TarballPass -> "tarball"
  ReleaseDeprecationPass -> "release_deprecation"
  RefreshLatestVersionsPass -> "refresh_latest_versions"
  MaintainersPass -> "maintainers"
