module Flora.Model.Requirement where

import qualified Crypto.Hash.MD5 as MD5
import Data.Aeson (FromJSON, ToJSON)
import Data.Data
import Data.Text (Text)
import Data.Text.Display
import Data.UUID (UUID, fromByteString)
import Database.PostgreSQL.Entity (Entity)
import Database.PostgreSQL.Entity.Types (GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField
  ( FromField,
    fromField,
    fromJSONField,
  )
import Database.PostgreSQL.Simple.ToField (ToField, toField, toJSONField)
import GHC.Generics (Generic)

import Data.ByteString.Lazy
import Data.Maybe (fromJust)
import Data.Text.Encoding (encodeUtf8)
import Flora.Model.Package.Component
import Flora.Model.Package.Types

newtype RequirementId = RequirementId {getRequirementId :: UUID}
  deriving
    (Eq, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID
  deriving (Display) via ShowInstance UUID

deterministicRequirementId :: ComponentId -> PackageId -> RequirementId
deterministicRequirementId componentId packageId =
  RequirementId . fromJust . fromByteString . fromStrict . MD5.hash . encodeUtf8 $ concatenated
  where
    concatenated = display componentId <> display packageId

data Requirement = Requirement
  { requirementId :: RequirementId
  -- ^ Unique identifier to this requirement in the database
  , packageComponentId :: ComponentId
  -- ^ Package component that depends on this requirement
  , packageId :: PackageId
  -- ^ Package that is being depended on
  , requirement :: Text
  -- ^ The human-readable version range expression of this requirement
  , metadata :: RequirementMetadata
  -- ^ Additional metadata, like flags
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "requirements"] Requirement)
  deriving
    (Display)
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
