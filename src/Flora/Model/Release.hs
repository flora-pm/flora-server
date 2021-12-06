{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
module Flora.Model.Release where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector
import Database.PostgreSQL.Entity (Entity, _selectWhere, insert,
                                   selectManyByField)
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Entity.Internal.QQ
import Database.PostgreSQL.Entity.Types (GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact (DBT)
import Distribution.Types.Version (Version)
import GHC.Generics (Generic)

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

getReleases :: PackageId -> DBT IO (Vector Release)
getReleases pid = selectManyByField @Release [field| package_id |] (Only pid)

getReleaseByVersion :: PackageId -> Version -> DBT IO (Maybe Release)
getReleaseByVersion packageId version = queryOne Select querySpec (packageId, version)
  where
    querySpec = _selectWhere @Release [ [field| package_id |], [field| version |]]
