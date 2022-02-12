{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Flora.Model.Release.Query where

import Control.Monad.IO.Class
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Transact (DBT)
import Distribution.Make (Version)

import Flora.Model.Package.Types (PackageId)
import Flora.Model.Release (Release)

getReleases :: MonadIO m => PackageId -> DBT m (Vector Release)
getReleases pid = selectManyByField @Release [field| package_id |] (Only pid)

getReleaseByVersion :: MonadIO m => PackageId -> Version -> DBT m (Maybe Release)
getReleaseByVersion packageId version = queryOne Select querySpec (packageId, version)
  where
    querySpec = _selectWhere @Release [ [field| package_id |], [field| version |]]
