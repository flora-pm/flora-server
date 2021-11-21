{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}

module Flora.Model.Package.Query where

import Data.Vector (Vector)
import Database.PostgreSQL.Entity (_select, _selectWhere, selectById)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query, queryOne,
                                       query_)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import Flora.Model.Package (Namespace, Package, PackageId, PackageName)

getAllPackages :: DBT IO (Vector Package)
getAllPackages = query_ Select (_select @Package)

getPackageById :: PackageId -> DBT IO (Maybe Package)
getPackageById packageId = selectById @Package (Only packageId)

getPackageByNamespaceAndName :: Namespace -> PackageName -> DBT IO (Maybe Package)
getPackageByNamespaceAndName namespace name = queryOne Select
  (_selectWhere @Package [[field| namespace |], [field| name |]])
  (namespace, name)

-- | This function is to be used when in Hackage Compatibility Mode.
getHaskellOrHackagePackage :: PackageName -> DBT IO (Maybe Package)
getHaskellOrHackagePackage packageName = queryOne Select [sql|
  SELECT DISTINCT p."package_id",
                  p."namespace",
                  p."name",
                  p."synopsis",
                  p."metadata",
                  p."owner_id",
                  p."created_at",
                  p."updated_at"
  FROM "packages" AS p
  WHERE p."namespace" IN ('haskell', 'hackage')
    AND p."name" = ?
  |] (Only packageName)

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
