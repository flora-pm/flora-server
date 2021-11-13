{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
module Flora.Model.Package where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Transact (DBT)
import qualified Distribution.SPDX.License as SPDX
import GHC.Generics

import Flora.Model.Package.Orphans ()
import Flora.Model.User

newtype PackageId = PackageId { getPackageId :: UUID }
  deriving stock (Generic)
  deriving (Eq, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID

newtype PackageName = PackageName Text

data Package = Package
  { packageId :: PackageId
  , namespace :: Text
  , name      :: Text
  , synopsis  :: Text
  , license   :: SPDX.License
  , ownerId   :: UserId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Show,Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[TableName "packages"] Package)

data Dependant = Dependant
  { name        :: Text
  , namespace   :: Text
  , dependantId :: PackageId
  }
  deriving stock (Eq, Show,Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[TableName "dependants"] Dependant)

createPackage :: Package -> DBT IO ()
createPackage package = insert @Package package

getPackageById :: PackageId -> DBT IO (Maybe Package)
getPackageById packageId = selectById @Package (Only packageId)

getPackageByName :: Text -> DBT IO (Maybe Package)
getPackageByName name = selectOneByField [field| package_name |] (Only name)

deletePackage :: PackageId -> DBT IO ()
deletePackage packageId = delete @Package (Only packageId)

refreshDependants :: DBT IO ()
refreshDependants = void $ execute Update [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "dependants"|] ()

getPackageDependants ::Text -> Text -> DBT IO (Vector Package)
getPackageDependants name namespace = query Select [sql|
SELECT p."package_id",
       p."namespace",
       p."name",
       p."synopsis",
       p."license",
       p."owner_id",
       p."created_at",
       p."updated_at"
FROM   "packages" AS p
       INNER JOIN "dependants" AS dep
               ON p."package_id" = dep."dependant_id"
WHERE  dep."name" = ?
  AND  dep."namespace" = ?
  |] (name, namespace)
