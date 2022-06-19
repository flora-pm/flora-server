{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Package.Update where

import Control.Monad (unless, void)
import Control.Monad.IO.Class
import Database.PostgreSQL.Entity (Entity (fields), delete, insert, insertMany, upsert)
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)

import qualified Data.List as List
import Database.PostgreSQL.Entity.Internal.QQ
import Flora.Model.Package.Component (PackageComponent)
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types
import Flora.Model.Requirement (Requirement)
import Optics.Core

insertPackage :: (MonadIO m) => Package -> DBT m ()
insertPackage package = insert @Package package

upsertPackage :: (MonadIO m) => Package -> DBT m ()
upsertPackage package =
  case package ^. #status of
    UnknownPackage -> upsert @Package package [[field| owner_id |]]
    FullyImportedPackage ->
      upsert @Package
        package
        [ [field| updated_at |]
        , [field| status |]
        , [field| owner_id |]
        ]

deletePackage :: (MonadIO m) => (Namespace, PackageName) -> DBT m ()
deletePackage (namespace, packageName) = delete @Package (namespace, packageName)

refreshDependents :: (MonadIO m) => DBT m ()
refreshDependents = void $ execute Update [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "dependents"|] ()

insertPackageComponent :: (MonadIO m) => PackageComponent -> DBT m ()
insertPackageComponent = insert @PackageComponent

upsertPackageComponent :: (MonadIO m) => PackageComponent -> DBT m ()
upsertPackageComponent packageComponent = upsert @PackageComponent packageComponent (fields @PackageComponent)

bulkInsertPackageComponents :: (MonadIO m) => [PackageComponent] -> DBT m ()
bulkInsertPackageComponents = insertMany @PackageComponent

insertRequirement :: (MonadIO m) => Requirement -> DBT m ()
insertRequirement = insert @Requirement

upsertRequirement :: (MonadIO m) => Requirement -> DBT m ()
upsertRequirement req = upsert @Requirement req [[field| metadata |], [field| requirement |]]

bulkInsertRequirements :: (MonadIO m) => [Requirement] -> DBT m ()
bulkInsertRequirements requirements = unless (List.null requirements) $ insertMany @Requirement requirements
