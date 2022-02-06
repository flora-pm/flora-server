{-# LANGUAGE QuasiQuotes #-}
module Flora.Model.Requirement where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data
import Data.Text (Text)
import Data.Text.Display
import Data.UUID (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (Entity, insert)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query)
import Database.PostgreSQL.Entity.Types (GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, fromField,
                                             fromJSONField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField, toField, toJSONField)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Database.PostgreSQL.Transact (DBT)
import GHC.Generics (Generic)

import Flora.Model.Package.Component
import Flora.Model.Package.Types
import Flora.Model.Release (ReleaseId)

newtype RequirementId = RequirementId { getRequirementId :: UUID }
  deriving (Eq, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID
  deriving Display via ShowInstance UUID

data Requirement = Requirement
  { -- | Unique identifier o this requirement in the database
    requirementId      :: RequirementId
  , -- | Package component that depends on this requirement
    packageComponentId :: ComponentId
  , -- | Package that is being depended on
    packageId          :: PackageId
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

insertRequirement :: Requirement -> DBT IO ()
insertRequirement = insert @Requirement

getRequirements :: ReleaseId
                   -- ^ Id of the release for which we want the dependencies
                -> DBT IO (Vector (Namespace, PackageName, Text))
                   -- ^ Returns a vector of (Namespace, Name, Version requirement)
getRequirements relId = query Select
  [sql|
    select dependency.namespace, dependency.name, req.requirement from requirements as req
     inner join packages as dependency on dependency.package_id = req.package_id
     inner join package_components as pc ON pc.package_component_id = req.package_component_id
     inner join releases as rel on rel.release_id = pc.release_id
   where rel."release_id" = ?
  |] (Only relId)
