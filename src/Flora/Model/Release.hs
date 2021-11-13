module Flora.Model.Release where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Entity (Entity, insert)
import Database.PostgreSQL.Entity.Types (GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact (DBT)
import Distribution.Types.Version (Version)
import GHC.Generics (Generic)

import Data.ByteString
import Flora.Model.Package (PackageId)
import Flora.Model.Release.Orphans ()

newtype ReleaseId = ReleaseId { getReleaseId :: UUID }
  deriving (Eq, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID

data Release = Release
  { -- | The unique ID of this release
    releaseId       :: ReleaseId
    -- | The package ID to which this release is linked
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

insertRelease :: Release -> DBT IO ()
insertRelease = insert @Release
