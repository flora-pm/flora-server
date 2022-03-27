module Flora.Model.Requirement where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data
import Data.Text (Text)
import Data.Text.Display
import Data.UUID (UUID)
import Database.PostgreSQL.Entity (Entity)
import Database.PostgreSQL.Entity.Types (GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, fromField,
                                             fromJSONField)
import Database.PostgreSQL.Simple.ToField (ToField, toField, toJSONField)
import GHC.Generics (Generic)

import Flora.Model.Package.Component
import Flora.Model.Package.Types

newtype RequirementId = RequirementId { getRequirementId :: UUID }
  deriving (Eq, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID
  deriving Display via ShowInstance UUID

data Requirement = Requirement
  { -- | Unique identifier o this requirement in the database
    requirementId      :: RequirementId
  , -- | Package component that depends on this requirement
    packageComponentId :: ComponentId
    -- | Package that is being depended on
  , packageNamespace   :: Namespace

  , packageName        :: PackageName
  , -- | The human-readable version range expression of this requirement
    requirement        :: Text
  , -- | Additional metadata, like flags
    metadata           :: RequirementMetadata
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[TableName "requirements"] Requirement)
  deriving Display
    via ShowInstance Requirement

data RequirementMetadata = RequirementMetadata
  { flag :: Maybe Text
  }
  deriving stock (Eq, Show, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON)

instance FromField RequirementMetadata where
  fromField = fromJSONField

instance ToField RequirementMetadata where
  toField = toJSONField
