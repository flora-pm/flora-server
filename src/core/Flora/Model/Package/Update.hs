{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Package.Update
  ( upsertPackageWithDependencies
  , packageInsertOrder
  , upsertPackage
  , bulkUpsertRequirements
  , insertRequirement
  , upsertPackageComponents
  , insertPackageComponent
  , refreshDependents
  , deprecatePackages
  ) where

import Control.Monad (void)
import Data.Function ((&))
import Data.List qualified as List
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

-- | Insert the package being imported together with the 'UnknownPackage'
-- skeletons of the packages it depends on, promoting it to
-- 'FullyImportedPackage' if that is its status.
--
-- The rows go in as a single statement ordered by 'PackageId'.
upsertPackageWithDependencies
  :: (IOE :> es, RequireCallStack, WriteDB :> es)
  => Package
  -- ^ The package being imported
  -> [Package]
  -- ^ 'UnknownPackage' skeletons for the packages it depends on
  -> Eff es ()
upsertPackageWithDependencies package dependencies =
  E.catch
    ( do
        void $
          executeMany
            (_insert @Package <> " ON CONFLICT DO NOTHING")
            (packageInsertOrder package dependencies)
        case package.status of
          UnknownPackage -> pure ()
          FullyImportedPackage ->
            void $
              execute
                (_updateFieldsBy @Package [[field| status |]] [field| package_id |])
                (toRow (Only package.status) ++ toRow (Only package.packageId))
    )
    (\sqlError@(SqlError{}) -> E.throwIO $ sqlErrorToDBException sqlError)

-- | The rows of 'upsertPackageWithDependencies' are deduplicated and in order.
--
-- The package being imported goes last so that 'dedupOn' keeps it over an
-- 'UnknownPackage' skeleton of itself, which a test suite depending on its own
-- library puts among the dependencies.
packageInsertOrder :: Package -> [Package] -> [Package]
packageInsertOrder package dependencies =
  dedupOn (.packageId) (dependencies <> [package])

{-# WARNING in "x-flora-test-only" upsertPackage "Exported for tests only" #-}
upsertPackage :: (IOE :> es, RequireCallStack, WriteDB :> es) => Package -> Eff es ()
upsertPackage package = upsertPackageWithDependencies package []

-- | Keep the last occurrence of each element by key, ordered by that key.
--
-- Both properties matter on the import path: the ordering is what stops
-- concurrent importers from taking row locks in different orders.
dedupOn :: Ord k => (a -> k) -> [a] -> [a]
dedupOn keyFun = Map.elems . Map.fromList . List.map (\x -> (keyFun x, x))

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

refreshDependents :: (IOE :> es, RequireCallStack, WriteDB :> es) => Eff es ()
refreshDependents =
  void $ execute [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "dependents"|] ()

insertPackageComponent :: (IOE :> es, RequireCallStack, WriteDB :> es) => PackageComponent -> Eff es ()
insertPackageComponent pc = void $ execute (_insert @PackageComponent) pc

upsertPackageComponents :: (IOE :> es, RequireCallStack, WriteDB :> es) => [PackageComponent] -> Eff es ()
upsertPackageComponents packageComponents =
  void $ executeMany (_insert @PackageComponent <> " ON CONFLICT DO NOTHING") packageComponents

insertRequirement :: (IOE :> es, RequireCallStack, WriteDB :> es) => Requirement -> Eff es ()
insertRequirement req = void $ execute (_insert @Requirement) req

bulkUpsertRequirements :: (IOE :> es, RequireCallStack, WriteDB :> es) => [Requirement] -> Eff es ()
bulkUpsertRequirements requirements =
  upsertMany @Requirement deduped [[field| components |], [field| requirement |]]
  where
    deduped = dedupOn (.requirementId) requirements
