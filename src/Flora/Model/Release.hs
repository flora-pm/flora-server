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

newtype ReleaseId = ReleaseId { getReleaseId :: UUID }
  deriving (Eq, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID
  deriving Display
    via ShowInstance UUID

data Release = Release
  { -- | The unique ID of this release
    releaseId       :: ReleaseId
    -- | The package to which this release is linked
  , packageId       :: PackageId
    -- | The version that this release represents
  , version         :: Version
    -- | The SHA256 checksum of the stored archive for this release
  , archiveChecksum :: ByteString
    -- | Date of creation of this release
  , createdAt       :: UTCTime
    -- | Last update timestamp for this release
  , updatedAt       :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[TableName "releases"] Release)
