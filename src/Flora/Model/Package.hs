{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
module Flora.Model.Package
  ( Package(..)
  , PackageId(..)
  , PackageName
  , Namespace
  , PackageMetadata(..)
  , createPackage
  , getPackageById
  , getPackageByNamespaceAndName
  , getPackageDependents
  , refreshDependents
  , deletePackage
  ) where

import Control.Monad (void)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (_selectWhere, delete, insert, selectById)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select, Update), execute,
                                       query, queryOne)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)

import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types

createPackage :: Package -> DBT IO ()
createPackage package = insert @Package package

getPackageById :: PackageId -> DBT IO (Maybe Package)
getPackageById packageId = selectById @Package (Only packageId)

getPackageByNamespaceAndName :: Namespace -> PackageName -> DBT IO (Maybe Package)
getPackageByNamespaceAndName namespace name = queryOne Select
  (_selectWhere @Package [[field| namespace |], [field| name |]])
  (namespace, name)

deletePackage :: PackageId -> DBT IO ()
deletePackage packageId = delete @Package (Only packageId)

refreshDependents :: DBT IO ()
refreshDependents = void $ execute Update [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "dependents"|] ()

-- | Remove the manual fields and use pg-entity
getPackageDependents :: Namespace
                     -> PackageName
                     -> DBT IO (Vector Package)
getPackageDependents namespace name  = query Select [sql|
SELECT DISTINCT p."package_id",
                p."namespace",
                p."name",
                p."synopsis",
                p."metadata",
                p."owner_id",
                p."created_at",
                p."updated_at"
FROM   "packages" AS p
       INNER JOIN "dependents" AS dep
               ON p."package_id" = dep."dependent_id"
WHERE  dep."namespace" = ?
  AND  dep."name" = ?
  |] (namespace, name)
