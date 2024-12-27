module Advisories.Model.Affected.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.ToField
import Distribution.Types.Version
import Distribution.Types.VersionRange (VersionRange)
import GHC.Generics
import Security.Advisories.Core.Advisory
import Security.Advisories.Core.HsecId
import Security.CVSS (CVSS)

import Advisories.AffectedVersionRange.Orphans ()
import Advisories.CVSS.Orphans ()
import Advisories.Model.Advisory.Types
import Advisories.System.Orphans ()
import Distribution.Orphans.ConfVar ()
import Distribution.Orphans.Version ()
import Flora.Model.Package.Types

newtype AffectedPackageId = AffectedPackageId {getAffectedPackageId :: UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, FromJSON, ToJSON, FromField, ToField, NFData)
    via UUID

data AffectedPackageDAO = AffectedPackageDAO
  { affectedPackageId :: AffectedPackageId
  , advisoryId :: AdvisoryId
  , packageId :: PackageId
  , cvss :: CVSS
  , architectures :: Maybe (Vector Architecture)
  , operatingSystems :: Maybe (Vector OS)
  , declarations :: Vector AffectedDeclaration
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[TableName "affected_packages"] AffectedPackageDAO)

data AffectedDeclaration = AffectedDeclaration
  { canonicalPath :: Text
  , affectedRange :: VersionRange
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

deriving via (Aeson AffectedDeclaration) instance ToField AffectedDeclaration

deriving via (Aeson AffectedDeclaration) instance FromField AffectedDeclaration

newtype AffectedVersionId = AffectedVersionId {getAffectedVersionId :: UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, FromJSON, ToJSON, FromField, ToField, NFData)
    via UUID

data AffectedVersionRangeDAO = AffectedVersionRangeDAO
  { affectedVersionId :: AffectedVersionId
  , affectedPackageId :: AffectedPackageId
  , introducedVersion :: Version
  , fixedVersion :: Maybe Version
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[TableName "affected_version_ranges"] AffectedVersionRangeDAO)

data PackageAdvisoryPreview = PackageAdvisoryPreview
  { hsecId :: HsecId
  , summary :: Text
  , fixed :: Bool
  , published :: UTCTime
  , cvss :: CVSS
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, NFData)
