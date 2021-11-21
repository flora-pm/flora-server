{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
module Flora.Model.Package.Update where

import Control.Monad (void)
import Database.PostgreSQL.Entity (delete, insert)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)

import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute)
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types

insertPackage :: Package -> DBT IO ()
insertPackage package = insert @Package package

deletePackage :: PackageId -> DBT IO ()
deletePackage packageId = delete @Package (Only packageId)

refreshDependents :: DBT IO ()
refreshDependents = void $ execute Update [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "dependents"|] ()
