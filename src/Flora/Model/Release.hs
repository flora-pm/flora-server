module Flora.Release where

import Flora.Package
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Aeson
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.UUID
import GHC.Generics
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Entity.Types

newtype ReleaseId = ReleaseId { getReleaseId :: UUID }
  deriving (Eq, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID

data Release = Release
  { releaseId :: ReleaseId
  , packageId :: PackageId
  , version   :: Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[TableName "organisations"] Release)
