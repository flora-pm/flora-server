{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
module Flora.Model.Package
  ( Package(..)
  , PackageId(..)
  , PackageMetadata(..)
  , createPackage
  , getPackageById
  , getPackageByNamespaceAndName
  , getPackageDependants
  , refreshDependants
  , deletePackage
  ) where

import Control.Monad
import Data.Text (Text)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Transact (DBT)

import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types (Package (..), PackageId (..),
                                  PackageMetadata (..))

createPackage :: Package -> DBT IO ()
createPackage package = insert @Package package

getPackageById :: PackageId -> DBT IO (Maybe Package)
getPackageById packageId = selectById @Package (Only packageId)

getPackageByNamespaceAndName :: Text -> Text -> DBT IO (Maybe Package)
getPackageByNamespaceAndName namespace name = queryOne Select
  (_selectWhere @Package [[field| namespace |], [field| name |]])
  (namespace, name)

deletePackage :: PackageId -> DBT IO ()
deletePackage packageId = delete @Package (Only packageId)

refreshDependants :: DBT IO ()
refreshDependants = void $ execute Update [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "dependants"|] ()

-- | Remove the manual fields and use pg-entity
getPackageDependants :: Text -- ^ Package namespace
                     -> Text -- ^ Package name
                     -> DBT IO (Vector Package)
getPackageDependants name namespace = query Select [sql|
SELECT p."package_id",
       p."namespace",
       p."name",
       p."synopsis",
       p."metadata",
       p."owner_id",
       p."created_at",
       p."updated_at"
FROM   "packages" AS p
       INNER JOIN "dependants" AS dep
               ON p."package_id" = dep."dependant_id"
WHERE  dep."name" = ?
  AND  dep."namespace" = ?
  |] (name, namespace)
