{-# LANGUAGE QuasiQuotes #-}
module Flora.Model.Requirement where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (Entity, insert)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query)
import Database.PostgreSQL.Entity.Types (GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Database.PostgreSQL.Transact (DBT)
import GHC.Generics (Generic)

import Flora.Model.Release (ReleaseId)
import Flora.Model.Package.Types

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

getRequirements :: ReleaseId -- ^ Id of the release for which we want the dependencies
                -> DBT IO (Vector (Namespace, PackageName, Text)) -- ^ Returns a vector of (Namespace, Name, Version requirement)
getRequirements relId = query Select
  [sql|
  select distinct p.namespace, p.name, req.requirement
    from requirements as req
    inner join packages as p
      on req.package_id = p.package_id
    inner join releases as rel
      on rel.release_id = req.release_id
    inner join packages as p2
      on p2."package_id" = rel."package_id"
  where req.release_id = ?
  |] (Only relId)
