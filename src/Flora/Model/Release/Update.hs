{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Release.Update where

import Control.Monad.IO.Class
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Transact (DBT)

import Control.Monad (void)
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Flora.Model.Release (Release)

insertRelease :: MonadIO m => Release -> DBT m ()
insertRelease = insert @Release

upsertRelease :: MonadIO m => Release -> DBT m ()
upsertRelease release = upsert @Release release [[field| updated_at |]]

refreshLatestVersions :: MonadIO m => DBT m ()
refreshLatestVersions = void $ execute Update [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "latest_versions" |] ()
