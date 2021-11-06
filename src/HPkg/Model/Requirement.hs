module HPkg.Release.Requirement where

import HPkg.Package
import Data.Time (UTCTime)
import Data.Aeson
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.UUID
import GHC.Generics
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Entity.Types
import HPkg.Release (ReleaseId)

newtype RequirementId = RequirementId { getRequirementId :: UUID }
  deriving (Eq, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID

data Requirement = Requirement
  { requirementId :: RequirementId
  , releaseId :: ReleaseId
  , packageId :: PackageId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[TableName "organisations"] Requirement)
