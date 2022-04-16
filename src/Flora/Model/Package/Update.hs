{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Package.Update where

import Control.Monad (unless, void)
import Control.Monad.IO.Class
import Database.PostgreSQL.Entity (delete, insert, insertMany)
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)

import qualified Data.List as List
import Flora.Model.Package.Component (PackageComponent)
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types
import Flora.Model.Requirement (Requirement)

insertPackage :: (MonadIO m) => Package -> DBT m ()
insertPackage package = insert @Package package

deletePackage :: (MonadIO m) => (Namespace, PackageName) -> DBT m ()
deletePackage (namespace, packageName) = delete @Package (namespace, packageName)

refreshDependents :: (MonadIO m) => DBT m ()
refreshDependents = void $ execute Update [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "dependents"|] ()

insertPackageComponent :: (MonadIO m) => PackageComponent -> DBT m ()
insertPackageComponent = insert @PackageComponent

bulkInsertPackageComponents :: (MonadIO m) => [PackageComponent] -> DBT m ()
bulkInsertPackageComponents = insertMany @PackageComponent

insertRequirement :: (MonadIO m) => Requirement -> DBT m ()
insertRequirement = insert @Requirement

bulkInsertRequirements :: (MonadIO m) => [Requirement] -> DBT m ()
bulkInsertRequirements requirements = unless (List.null requirements) $ insertMany @Requirement requirements
