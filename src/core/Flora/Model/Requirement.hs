module Flora.Model.Requirement where

import Control.DeepSeq
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable (foldl')
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Display
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.UUID (UUID, fromByteString)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (Entity)
import Database.PostgreSQL.Entity.Types (GenericEntity, TableName)
import Database.PostgreSQL.Simple (ToRow)
import Database.PostgreSQL.Simple.FromField
  ( FromField
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.Version (Version)

import Flora.Model.Component.Types
import Flora.Model.Package.Types

newtype RequirementId = RequirementId {getRequirementId :: UUID}
  deriving (Display) via ShowInstance UUID
  deriving
    (Eq, FromField, FromJSON, NFData, Show, ToField, ToJSON)
    via UUID

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
  , components :: Vector Text
  -- ^ Components that are depended on
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, FromRow, NFData, ToJSON, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "requirements"] Requirement)
  deriving
    (Display)
    via ShowInstance Requirement

-- | This datatype holds information about the latest version of a dependency
data DependencyInfo = DependencyInfo
  { namespace :: Namespace
  , name :: PackageName
  , requirement :: Text
  , components :: Vector Text
  , latestVersion :: Version
  , latestSynopsis :: Text
  , latestLicense :: SPDX.License
  , uploadedAt :: Maybe UTCTime
  , revisedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData)

-- | Data Access Object for component dependencies to read from db
data ComponentDependency' = ComponentDependency'
  { componentType :: ComponentType
  , componentName :: Text
  , namespace :: Namespace
  , name :: PackageName
  , requirement :: Text
  , components :: Vector Text
  , latestVersion :: Version
  , latestSynopsis :: Text
  , latestLicense :: SPDX.License
  , uploadedAt :: Maybe UTCTime
  , revisedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData)

-- | Map of components to its dependencies
type ComponentDependencies = Map.Map CanonicalComponent (Vector DependencyInfo)

toComponentDependencies :: Vector ComponentDependency' -> ComponentDependencies
toComponentDependencies = foldl' go Map.empty
  where
    go acc ComponentDependency'{..} =
      Map.insertWith (<>) (CanonicalComponent{..}) (pure DependencyInfo{..}) acc
