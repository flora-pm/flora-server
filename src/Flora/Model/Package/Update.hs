{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Package.Update where

import Control.Monad (unless, void)
import Database.PostgreSQL.Entity (Entity (fields), delete, insert, insertMany, upsert)
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute)
import Database.PostgreSQL.Simple.SqlQQ (sql)

import qualified Data.List as List
import Database.PostgreSQL.Entity.Internal.QQ
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Flora.Model.Package.Component (PackageComponent)
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types
import Flora.Model.Requirement (Requirement)
import Optics.Core

insertPackage :: ([DB, IOE] :>> es) => Package -> Eff es ()
insertPackage package = dbtToEff $ insert @Package package

upsertPackage :: ([DB, IOE] :>> es) => Package -> Eff es ()
upsertPackage package = dbtToEff $
  case package ^. #status of
    UnknownPackage -> upsert @Package package [[field| owner_id |]]
    FullyImportedPackage ->
      upsert @Package
        package
        [ [field| updated_at |]
        , [field| status |]
        , [field| owner_id |]
        ]

deletePackage :: ([DB, IOE] :>> es) => (Namespace, PackageName) -> Eff es ()
deletePackage (namespace, packageName) = dbtToEff $ delete @Package (namespace, packageName)

refreshDependents :: ([DB, IOE] :>> es) => Eff es ()
refreshDependents = dbtToEff $ void $ execute Update [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "dependents"|] ()

insertPackageComponent :: ([DB, IOE] :>> es) => PackageComponent -> Eff es ()
insertPackageComponent = dbtToEff . insert @PackageComponent

upsertPackageComponent :: ([DB, IOE] :>> es) => PackageComponent -> Eff es ()
upsertPackageComponent packageComponent = dbtToEff $ upsert @PackageComponent packageComponent (fields @PackageComponent)

bulkInsertPackageComponents :: ([DB, IOE] :>> es) => [PackageComponent] -> Eff es ()
bulkInsertPackageComponents = dbtToEff . insertMany @PackageComponent

insertRequirement :: ([DB, IOE] :>> es) => Requirement -> Eff es ()
insertRequirement = dbtToEff . insert @Requirement

upsertRequirement :: ([DB, IOE] :>> es) => Requirement -> Eff es ()
upsertRequirement req = dbtToEff $ upsert @Requirement req [[field| metadata |], [field| requirement |]]

bulkInsertRequirements :: ([DB, IOE] :>> es) => [Requirement] -> Eff es ()
bulkInsertRequirements requirements = dbtToEff $ unless (List.null requirements) $ insertMany @Requirement requirements
