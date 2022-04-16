{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Release.Query where

import Control.Monad.IO.Class
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), queryOne)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import Distribution.Make (Version)

import Flora.Model.Package.Types
import Flora.Model.Release (Release)

getReleases :: MonadIO m => PackageId -> DBT m (Vector Release)
getReleases pid = selectManyByField @Release [field| package_id |] (Only pid)

getReleaseByVersion :: MonadIO m => PackageId -> Version -> DBT m (Maybe Release)
getReleaseByVersion packageId version = queryOne Select querySpec (packageId, version)
  where
    querySpec =
      [sql|
        select r.release_id, r.package_id, r.version, r.archive_checksum, r.created_at, r.updated_at
        from releases as r
        where r.package_id = ?
          and r.version = ?
      |]
