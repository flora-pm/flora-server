module Flora.Model.Release where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Entity.Types (Entity, GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Distribution.Types.Version (Version)
import GHC.Generics (Generic)

import Data.Text.Display
import Flora.Model.Package
import Flora.Model.Release.Orphans ()

newtype ReleaseId = ReleaseId {getReleaseId :: UUID}
  deriving
    (Eq, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID
  deriving
    (Display)
    via ShowInstance UUID

data Release = Release
  { releaseId :: ReleaseId
  -- ^ The unique ID of this release
  , packageId :: PackageId
  -- ^ The package to which this release is linked
  , version :: Version
  -- ^ The version that this release represents
  , archiveChecksum :: ByteString
  -- ^ The SHA256 checksum of the stored archive for this release
  , createdAt :: UTCTime
  -- ^ Date of creation of this release
  , updatedAt :: UTCTime
  -- ^ Last update timestamp for this release
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "releases"] Release)
