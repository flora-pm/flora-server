{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}

module Flora.Model.Release.Query where

import Control.Monad.IO.Class
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query, queryOne)
import Database.PostgreSQL.Simple ()
import Database.PostgreSQL.Transact (DBT)
import Distribution.Make (Version)

import Database.PostgreSQL.Simple.SqlQQ (sql)
import Flora.Model.Package.Types
import Flora.Model.Release (Release)

getReleases :: MonadIO m => (Namespace, PackageName) -> DBT m (Vector Release)
getReleases (namespace, packageName) = query Select
  [sql|
    select r.release_id, r.package_namespace, r.package_name, r.version, r.archive_checksum, r.created_at, r.updated_at
    from releases as r
    where r.package_namespace = ?
      and r.package_name = ?
    |] (namespace, packageName)

getReleaseByVersion :: MonadIO m => (Namespace, PackageName) -> Version -> DBT m (Maybe Release)
getReleaseByVersion (namespace, packageName) version = queryOne Select querySpec (namespace, packageName, version)
  where
    querySpec =
      [sql|
        select r.release_id, r.package_namespace, r.package_name, r.version, r.archive_checksum, r.created_at, r.updated_at
        from releases as r
        where r.package_namespace = ?
          and r.package_name = ?
          and r.version = ?
      |]
