{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Package.Update where

import Control.Monad (unless, void)
import Data.Function ((&))
import Data.List qualified as List
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity (Entity (fields), delete, insert, insertMany, upsert)
import Database.PostgreSQL.Entity.DBT (execute, executeMany)
import Database.PostgreSQL.Entity.Internal.QQ
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import RequireCallStack

import Flora.Model.Component.Types (PackageComponent)
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types
import Flora.Model.Requirement (Requirement)

upsertPackage :: (DB :> es, RequireCallStack) => Package -> Eff es ()
upsertPackage package =
  dbtToEff $
    case package.status of
      UnknownPackage -> upsert @Package package [[field| updated_at |]]
      FullyImportedPackage ->
        upsert @Package
          package
          [ [field| updated_at |]
          , [field| status |]
          ]

deprecatePackages :: (DB :> es, RequireCallStack) => Vector DeprecatedPackage -> Eff es ()
deprecatePackages dp = dbtToEff $ void $ executeMany q (dp & Vector.map Only & Vector.toList)
  where
    q =
      [sql|
      UPDATE packages as p0
      SET deprecation_info = jsonb(js) -> 'in_favour_of'
      FROM (VALUES (?)) as upd (js)
      WHERE p0.name = jsonb(js) ->> 'package'
      |]

deletePackage :: (DB :> es, RequireCallStack) => (Namespace, PackageName) -> Eff es ()
deletePackage (namespace, packageName) = dbtToEff $ delete @Package (namespace, packageName)

refreshDependents :: (DB :> es, RequireCallStack) => Eff es ()
refreshDependents =
  dbtToEff $ void $ execute [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "dependents"|] ()

insertPackageComponent :: (DB :> es, RequireCallStack) => PackageComponent -> Eff es ()
insertPackageComponent = dbtToEff . insert @PackageComponent

upsertPackageComponent :: (DB :> es, RequireCallStack) => PackageComponent -> Eff es ()
upsertPackageComponent packageComponent =
  dbtToEff $ upsert @PackageComponent packageComponent (fields @PackageComponent)

bulkInsertPackageComponents :: (DB :> es, RequireCallStack) => [PackageComponent] -> Eff es ()
bulkInsertPackageComponents = dbtToEff . insertMany @PackageComponent

insertRequirement :: (DB :> es, RequireCallStack) => Requirement -> Eff es ()
insertRequirement = dbtToEff . insert @Requirement

upsertRequirement :: (DB :> es, RequireCallStack) => Requirement -> Eff es ()
upsertRequirement req = dbtToEff $ upsert @Requirement req [[field| components |], [field| requirement |]]

bulkInsertRequirements :: (DB :> es, RequireCallStack) => [Requirement] -> Eff es ()
bulkInsertRequirements requirements =
  dbtToEff $ unless (List.null requirements) $ insertMany @Requirement requirements
