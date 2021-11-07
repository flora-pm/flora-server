module Flora.Model.Requirement where

import Data.Aeson
import Data.Text (Text)
import Data.UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact
import GHC.Generics

import Flora.Model.Package
import Flora.Model.Release (Release, ReleaseId)

newtype RequirementId = RequirementId { getRequirementId :: UUID }
  deriving (Eq, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID

data Requirement = Requirement
  { requirementId :: RequirementId
  , releaseId     :: ReleaseId
  , packageId     :: PackageId
  , requirement   :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[TableName "requirements"] Requirement)

insertRequirement :: Requirement -> DBT IO ()
insertRequirement = insert @Requirement

getRequirements :: Release -> DBT IO ()
getRequirements = undefined
