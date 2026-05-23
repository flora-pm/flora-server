{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Package.Update where

import Control.Monad (unless, void)
import Data.Function ((&))
import Data.List qualified as List
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity hiding (upsert)
import Database.PostgreSQL.Entity.Internal.QQ
import Database.PostgreSQL.Simple (Only (..), SqlError (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToRow
import Effectful
import Effectful.Exception qualified as E
import Effectful.Labeled
import Effectful.PostgreSQL
import RequireCallStack

import Flora.DB.Exception
import Flora.Database
import Flora.Model.Component.Types (PackageComponent)
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types
import Flora.Model.Requirement (Requirement)

upsertPackage :: (IOE :> es, Labeled ReadWrite WithConnection :> es, RequireCallStack) => Package -> Eff es ()
upsertPackage package =
  E.catch
    ( labeled @ReadWrite @WithConnection $ do
        upsertWith package
        case package.status of
          UnknownPackage -> pure ()
          FullyImportedPackage ->
            void $
              execute
                (_updateFieldsBy @Package [[field| status |]] [field| package_id |])
                (toRow (Only package.status) ++ toRow (Only package.packageId))
    )
    (\sqlError@(SqlError{}) -> E.throwIO $ sqlErrorToDBException sqlError)
  where
    upsertWith entity =
      void $ execute (_insert @Package <> " ON CONFLICT DO NOTHING") entity

deprecatePackages :: (IOE :> es, Labeled ReadWrite WithConnection :> es, RequireCallStack) => Vector DeprecatedPackage -> Eff es ()
deprecatePackages dp = labeled @ReadWrite @WithConnection $ void $ executeMany q (dp & Vector.map Only & Vector.toList)
  where
    q =
      [sql|
      UPDATE packages as p0
      SET deprecation_info = jsonb(js) -> 'in_favour_of'
      FROM (VALUES (?)) as upd (js)
      WHERE p0.name = jsonb(js) ->> 'package'
      |]

deletePackage :: (IOE :> es, Labeled ReadWrite WithConnection :> es, RequireCallStack) => (Namespace, PackageName) -> Eff es ()
deletePackage (namespace, packageName) = labeled @ReadWrite @WithConnection $ void $ execute (_deleteWhere @Package [primaryKey @Package]) (namespace, packageName)

refreshDependents :: (IOE :> es, Labeled ReadWrite WithConnection :> es, RequireCallStack) => Eff es ()
refreshDependents =
  labeled @ReadWrite @WithConnection $ void $ execute [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "dependents"|] ()

insertPackageComponent :: (IOE :> es, Labeled ReadWrite WithConnection :> es, RequireCallStack) => PackageComponent -> Eff es ()
insertPackageComponent pc = labeled @ReadWrite @WithConnection $ void $ execute (_insert @PackageComponent) pc

upsertPackageComponent :: (IOE :> es, Labeled ReadWrite WithConnection :> es, RequireCallStack) => PackageComponent -> Eff es ()
upsertPackageComponent packageComponent =
  labeled @ReadWrite @WithConnection $ upsert @PackageComponent packageComponent (fields @PackageComponent)

upsertPackageComponents :: (IOE :> es, Labeled ReadWrite WithConnection :> es, RequireCallStack) => [PackageComponent] -> Eff es ()
upsertPackageComponents packageComponents =
  labeled @ReadWrite @WithConnection $ void $ executeMany (_insert @PackageComponent <> " ON CONFLICT DO NOTHING") packageComponents

bulkInsertPackageComponents :: (IOE :> es, Labeled ReadWrite WithConnection :> es, RequireCallStack) => [PackageComponent] -> Eff es ()
bulkInsertPackageComponents pcs = labeled @ReadWrite @WithConnection $ void $ executeMany (_insert @PackageComponent) pcs

insertRequirement :: (IOE :> es, Labeled ReadWrite WithConnection :> es, RequireCallStack) => Requirement -> Eff es ()
insertRequirement req = labeled @ReadWrite @WithConnection $ void $ execute (_insert @Requirement) req

upsertRequirement :: (IOE :> es, Labeled ReadWrite WithConnection :> es, RequireCallStack) => Requirement -> Eff es ()
upsertRequirement req = labeled @ReadWrite @WithConnection $ upsert @Requirement req [[field| components |], [field| requirement |]]

bulkInsertRequirements :: (IOE :> es, Labeled ReadWrite WithConnection :> es, RequireCallStack) => [Requirement] -> Eff es ()
bulkInsertRequirements requirements =
  labeled @ReadWrite @WithConnection $ unless (List.null requirements) $ void (executeMany (_insert @Requirement) requirements)
