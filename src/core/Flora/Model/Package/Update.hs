{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Package.Update where

import Control.Monad (unless, void)
import Data.Function ((&))
import Data.List qualified as List
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (DBT, execute, executeMany)
import Database.PostgreSQL.Entity.Internal.QQ
import Database.PostgreSQL.Simple (Only (..), SqlError (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Effectful.Exception qualified as E
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import RequireCallStack

import Flora.DB.Exception
import Flora.Model.Component.Types (PackageComponent)
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types
import Flora.Model.Requirement (Requirement)

upsertPackage :: (DB :> es, RequireCallStack) => Package -> Eff es ()
upsertPackage package =
  E.catch
    ( dbtToEff $ do
        upsertWith package
        case package.status of
          UnknownPackage -> pure ()
          FullyImportedPackage ->
            void $
              updateFieldsBy @Package
                [[field| status |]]
                ([field| package_id |], package.packageId)
                (Only package.status)
    )
    (\sqlError@(SqlError{}) -> E.throwIO $ sqlErrorToDBException sqlError)
  where
    upsertWith :: Package -> DBT IO ()
    upsertWith entity =
      void $ execute @Package (_insert @Package <> " ON CONFLICT DO NOTHING") entity

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

upsertPackageComponents :: (DB :> es, RequireCallStack) => [PackageComponent] -> Eff es ()
upsertPackageComponents packageComponents =
  dbtToEff $ void $ executeMany (_insert @PackageComponent <> " ON CONFLICT DO NOTHING") packageComponents

bulkInsertPackageComponents :: (DB :> es, RequireCallStack) => [PackageComponent] -> Eff es ()
bulkInsertPackageComponents = dbtToEff . insertMany @PackageComponent

insertRequirement :: (DB :> es, RequireCallStack) => Requirement -> Eff es ()
insertRequirement = dbtToEff . insert @Requirement

upsertRequirement :: (DB :> es, RequireCallStack) => Requirement -> Eff es ()
upsertRequirement req = dbtToEff $ upsert @Requirement req [[field| components |], [field| requirement |]]

bulkInsertRequirements :: (DB :> es, RequireCallStack) => [Requirement] -> Eff es ()
bulkInsertRequirements requirements =
  dbtToEff $ unless (List.null requirements) $ insertMany @Requirement requirements
