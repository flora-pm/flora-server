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

import Flora.Import.Package.Types
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

data PackageJob
  = FetchReadme ReadmeJobPayload
  | FetchTarball TarballJobPayload
  | FetchUploadInformation UploadInformationJobPayload
  | FetchChangelog ChangelogJobPayload
  | ImportPackage ImportOutput
  | FetchPackageDeprecationList
  | FetchReleaseDeprecationList PackageName (Vector ReleaseId)
  | RefreshLatestVersions
  | RefreshIndex Text
  deriving stock (Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''PackageJob)
