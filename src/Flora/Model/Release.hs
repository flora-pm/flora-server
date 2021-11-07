module Flora.Model.Release where

import Data.Aeson
import Data.Time (UTCTime)
import Data.UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact
import Distribution.Types.Version (Version)
import GHC.Generics

import Flora.Model.Package
import Flora.Model.Release.Orphans ()

newtype ReleaseId = ReleaseId { getReleaseId :: UUID }
  deriving (Eq, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID

data Release = Release
  { releaseId :: ReleaseId
  , packageId :: PackageId
  , version   :: Version
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[TableName "releases"] Release)

insertRelease :: Release -> DBT IO ()
insertRelease = insert @Release
