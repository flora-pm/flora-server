{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Release.Update where

import Control.Monad (void)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Effectful.PostgreSQL.Transact.Effect

import Flora.Model.Release.Types (Release, ReleaseId, TextHtml (..))

insertRelease :: ([DB, IOE] :>> es) => Release -> Eff es ()
insertRelease = dbtToEff . insert @Release

upsertRelease :: ([DB, IOE] :>> es) => Release -> Eff es ()
upsertRelease release = dbtToEff $ upsert @Release release [[field| updated_at |]]

refreshLatestVersions :: ([DB, IOE] :>> es) => Eff es ()
refreshLatestVersions = dbtToEff $ void $ execute Update [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "latest_versions" |] ()

updateReadme :: ([DB, IOE] :>> es) => ReleaseId -> Maybe TextHtml -> Eff es ()
updateReadme releaseId readmeBody =
  dbtToEff $
    void $
      updateFieldsBy @Release
        [[field| readme |]]
        ([field| release_id |], releaseId)
        (Only readmeBody)
