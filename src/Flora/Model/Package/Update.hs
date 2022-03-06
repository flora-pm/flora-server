{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
module Flora.Model.Package.Update where

import Control.Monad (void)
import Database.PostgreSQL.Entity (delete, insert)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)

import Control.Monad.IO.Class
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute)
import Flora.Model.Package.Component (PackageComponent)
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types
import Flora.Model.Requirement (Requirement)

insertPackage :: (MonadIO m) => Package -> DBT m ()
insertPackage package = insert @Package package

deletePackage :: (MonadIO m) => PackageId -> DBT m ()
deletePackage packageId = delete @Package (Only packageId)

refreshDependents :: (MonadIO m) => DBT m ()
refreshDependents = void $ execute Update [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "dependents"|] ()

insertPackageComponent :: (MonadIO m) => PackageComponent -> DBT m ()
insertPackageComponent = insert @PackageComponent

insertRequirement :: (MonadIO m) => Requirement -> DBT m ()
insertRequirement = insert @Requirement
