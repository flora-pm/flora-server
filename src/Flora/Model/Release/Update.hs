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

import Data.Time (UTCTime)
import Flora.Model.Release.Types (ImportStatus (..), Release, ReleaseId, TextHtml (..))

insertRelease :: (DB :> es) => Release -> Eff es ()
insertRelease = dbtToEff . insert @Release

upsertRelease :: (DB :> es) => Release -> Eff es ()
upsertRelease release = dbtToEff $ upsert @Release release [[field| updated_at |]]

refreshLatestVersions :: (DB :> es) => Eff es ()
refreshLatestVersions = dbtToEff $ void $ execute Update [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "latest_versions" |] ()

updateReadme :: (DB :> es) => ReleaseId -> Maybe TextHtml -> ImportStatus -> Eff es ()
updateReadme releaseId readmeBody status =
  dbtToEff $
    void $
      updateFieldsBy @Release
        [ [field| readme |]
        , [field| readme_status |]
        ]
        ([field| release_id |], releaseId)
        (readmeBody, status)

updateUploadTime :: (DB :> es) => ReleaseId -> UTCTime -> Eff es ()
updateUploadTime releaseId timestamp =
  dbtToEff $
    void $
      updateFieldsBy @Release
        [[field| uploaded_at |]]
        ([field| release_id |], releaseId)
        (Only (Just timestamp))

updateChangelog :: (DB :> es) => ReleaseId -> Maybe TextHtml -> ImportStatus -> Eff es ()
updateChangelog releaseId changelogBody status =
  dbtToEff $
    void $
      updateFieldsBy @Release
        [ [field| changelog |]
        , [field| changelog_status |]
        ]
        ([field| release_id |], releaseId)
        (changelogBody, status)
