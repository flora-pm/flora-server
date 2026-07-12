{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Package.Update where

import Control.Monad (void)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity hiding (upsert)
import Database.PostgreSQL.Entity.Internal.QQ
import Database.PostgreSQL.Simple (Only (..), SqlError (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToRow
import Effectful
import Effectful.Exception qualified as E
import RequireCallStack

import Flora.DB.Exception
import Flora.Database
import Flora.Model.Component.Types (PackageComponent)
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types
import Flora.Model.Requirement (Requirement (..))

upsertPackage :: (IOE :> es, RequireCallStack, WriteDB :> es) => Package -> Eff es ()
upsertPackage package =
  E.catch
    ( do
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

-- | Keep the __last__ occurrence of each element by key (as 'Map.fromList'
-- does) to make a batch safe for a bulk insert.
dedupOn :: Ord k => (a -> k) -> [a] -> [a]
dedupOn key = Map.elems . Map.fromList . map (\x -> (key x, x))

-- | Insert many packages unknown packages.
-- This must not be used to promote a package to
-- 'FullyImportedPackage' (use 'upsertPackage' for that).
-- It is for inserting 'UnknownPackage' dependency skeletons without downgrading a package that is already known.
--
-- TODO: Probably should label Packages at the type level for their import level?
bulkInsertUnknownPackages :: (IOE :> es, RequireCallStack, WriteDB :> es) => [Package] -> Eff es ()
bulkInsertUnknownPackages packages =
  E.catch
    (void $ executeMany (_insert @Package <> " ON CONFLICT DO NOTHING") deduped)
    (\sqlError@(SqlError{}) -> E.throwIO $ sqlErrorToDBException sqlError)
  where
    deduped = dedupOn (.packageId) packages

deprecatePackages :: (IOE :> es, RequireCallStack, WriteDB :> es) => Vector DeprecatedPackage -> Eff es ()
deprecatePackages dp = void $ executeMany q (dp & Vector.map Only & Vector.toList)
  where
    q =
      [sql|
      UPDATE packages as p0
      SET deprecation_info = jsonb(js) -> 'in_favour_of'
      FROM (VALUES (?)) as upd (js)
      WHERE p0.name = jsonb(js) ->> 'package'
      |]

deletePackage :: (IOE :> es, RequireCallStack, WriteDB :> es) => (Namespace, PackageName) -> Eff es ()
deletePackage (namespace, packageName) = void $ execute (_deleteWhere @Package [primaryKey @Package]) (namespace, packageName)

refreshDependents :: (IOE :> es, RequireCallStack, WriteDB :> es) => Eff es ()
refreshDependents =
  void $ execute [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "dependents"|] ()

insertPackageComponent :: (IOE :> es, RequireCallStack, WriteDB :> es) => PackageComponent -> Eff es ()
insertPackageComponent pc = void $ execute (_insert @PackageComponent) pc

upsertPackageComponent :: (IOE :> es, RequireCallStack, WriteDB :> es) => PackageComponent -> Eff es ()
upsertPackageComponent packageComponent =
  upsert @PackageComponent packageComponent (fields @PackageComponent)

upsertPackageComponents :: (IOE :> es, RequireCallStack, WriteDB :> es) => [PackageComponent] -> Eff es ()
upsertPackageComponents packageComponents =
  void $ executeMany (_insert @PackageComponent <> " ON CONFLICT DO NOTHING") packageComponents

bulkInsertPackageComponents :: (IOE :> es, RequireCallStack, WriteDB :> es) => [PackageComponent] -> Eff es ()
bulkInsertPackageComponents pcs = void $ executeMany (_insert @PackageComponent) pcs

insertRequirement :: (IOE :> es, RequireCallStack, WriteDB :> es) => Requirement -> Eff es ()
insertRequirement req = void $ execute (_insert @Requirement) req

bulkUpsertRequirements :: (IOE :> es, RequireCallStack, WriteDB :> es) => [Requirement] -> Eff es ()
bulkUpsertRequirements requirements =
  upsertMany @Requirement deduped [[field| components |], [field| requirement |]]
  where
    deduped = dedupOn (.requirementId) requirements
