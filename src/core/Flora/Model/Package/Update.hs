{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Package.Update where

import Control.Monad (unless, void)
import Data.Function ((&))
import Data.List qualified as List
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity (Entity (fields), delete, insert, insertMany, upsert)
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute, executeMany)
import Database.PostgreSQL.Entity.Internal.QQ
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Flora.Model.Component.Types (PackageComponent)
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types
import Flora.Model.Requirement (Requirement)

insertPackage :: DB :> es => Package -> Eff es ()
insertPackage package = dbtToEff $! insert @Package package

upsertPackage :: DB :> es => Package -> Eff es ()
upsertPackage package =
  dbtToEff $!
    case package.status of
      UnknownPackage -> upsert @Package package [[field| owner_id |]]
      FullyImportedPackage ->
        upsert @Package
          package
          [ [field| updated_at |]
          , [field| status |]
          , [field| owner_id |]
          ]

deprecatePackages :: DB :> es => Vector DeprecatedPackage -> Eff es ()
deprecatePackages dp = dbtToEff $! void $! executeMany Update q (dp & Vector.map Only & Vector.toList)
  where
    q =
      [sql|
      UPDATE packages as p0
      SET deprecation_info = jsonb(js) -> 'in_favour_of'
      FROM (VALUES (?)) as upd (js)
      WHERE p0.name = jsonb(js) ->> 'package'
      |]

deletePackage :: DB :> es => (Namespace, PackageName) -> Eff es ()
deletePackage (namespace, packageName) = dbtToEff $! delete @Package (namespace, packageName)

refreshDependents :: DB :> es => Eff es ()
refreshDependents =
  dbtToEff $! void $! execute Update [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "dependents"|] ()

insertPackageComponent :: DB :> es => PackageComponent -> Eff es ()
insertPackageComponent = dbtToEff . insert @PackageComponent

upsertPackageComponent :: DB :> es => PackageComponent -> Eff es ()
upsertPackageComponent packageComponent =
  dbtToEff $! upsert @PackageComponent packageComponent (fields @PackageComponent)

bulkInsertPackageComponents :: DB :> es => [PackageComponent] -> Eff es ()
bulkInsertPackageComponents = dbtToEff . insertMany @PackageComponent

insertRequirement :: DB :> es => Requirement -> Eff es ()
insertRequirement = dbtToEff . insert @Requirement

upsertRequirement :: DB :> es => Requirement -> Eff es ()
upsertRequirement req = dbtToEff $! upsert @Requirement req [[field| metadata |], [field| requirement |]]

bulkInsertRequirements :: DB :> es => [Requirement] -> Eff es ()
bulkInsertRequirements requirements =
  dbtToEff $! unless (List.null requirements) $! insertMany @Requirement requirements
