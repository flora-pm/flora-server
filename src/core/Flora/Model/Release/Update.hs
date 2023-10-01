{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Release.Update where

import Control.Monad (void)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute, executeMany)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Effectful.PostgreSQL.Transact.Effect

import Data.Function ((&))
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Flora.Model.Release.Types

insertRelease :: DB :> es => Release -> Eff es ()
insertRelease = dbtToEff . insert @Release

upsertRelease :: DB :> es => Release -> Eff es ()
upsertRelease release = dbtToEff $ upsert @Release release [[field| updated_at |]]

refreshLatestVersions :: DB :> es => Eff es ()
refreshLatestVersions = dbtToEff $ void $ execute Update [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "latest_versions" |] ()

updateReadme :: DB :> es => ReleaseId -> Maybe TextHtml -> ImportStatus -> Eff es ()
updateReadme releaseId readmeBody status =
  dbtToEff $
    void $
      updateFieldsBy @Release
        [ [field| readme |]
        , [field| readme_status |]
        ]
        ([field| release_id |], releaseId)
        (readmeBody, status)

updateUploadTime :: DB :> es => ReleaseId -> UTCTime -> Eff es ()
updateUploadTime releaseId timestamp =
  dbtToEff $
    void $
      updateFieldsBy @Release
        [[field| uploaded_at |]]
        ([field| release_id |], releaseId)
        (Only (Just timestamp))

updateRevisionTime :: DB :> es => ReleaseId -> UTCTime -> Eff es ()
updateRevisionTime releaseId timestamp =
  dbtToEff $
    void $
      updateFieldsBy @Release
        [[field| revised_at |]]
        ([field| release_id |], releaseId)
        (Only (Just timestamp))

updateChangelog :: DB :> es => ReleaseId -> Maybe TextHtml -> ImportStatus -> Eff es ()
updateChangelog releaseId changelogBody status =
  dbtToEff $
    void $
      updateFieldsBy @Release
        [ [field| changelog |]
        , [field| changelog_status |]
        ]
        ([field| release_id |], releaseId)
        (changelogBody, status)

setReleasesDeprecationMarker :: DB :> es => Vector (Bool, ReleaseId) -> Eff es ()
setReleasesDeprecationMarker releaseVersions =
  dbtToEff $ void $ executeMany Update q (releaseVersions & Vector.toList)
  where
    q =
      [sql|
    UPDATE releases as r0
    SET deprecated = upd.x
    FROM (VALUES (?,?)) as upd(x,y)
    WHERE r0.release_id = (upd.y :: uuid) 
    |]
