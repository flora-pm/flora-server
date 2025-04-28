{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Release.Update where

import Control.Monad (void, when)
import Crypto.Hash.SHA256 qualified as SHA
import Data.Aeson
import Data.ByteString (toStrict)
import Data.ByteString.Lazy (LazyByteString)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, executeMany)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Distribution.Types.Version (Version)
import Effectful
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time (Time)
import Log qualified

import Flora.Environment.Env (DeploymentEnv (..), FloraEnv (..))
import Flora.Model.BlobStore.API (BlobStoreAPI, put)
import Flora.Model.BlobStore.Types
import Flora.Model.Feed.Types qualified as Types
import Flora.Model.Feed.Update qualified as Update
import Flora.Model.Package.Types (Package (..))
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types

insertRelease :: DB :> es => Release -> Eff es ()
insertRelease = dbtToEff . insert @Release

upsertRelease :: (DB :> es, IOE :> es, Log :> es, Reader FloraEnv :> es, Time :> es) => Package -> Release -> Eff es ()
upsertRelease package newRelease = do
  mReleaseFromDB <- Query.getReleaseById newRelease.releaseId
  case mReleaseFromDB of
    Just releaseFromDB ->
      when (releaseFromDB.testedWith == newRelease.testedWith) $ do
        Log.logAttention "Duplicate releases found" $
          object
            [ "new_release" .= newRelease
            , "release_from_db" .= releaseFromDB
            ]
        updateTestedWith
          newRelease.releaseId
          newRelease.testedWith
          newRelease.updatedAt
    Nothing -> do
      Log.logInfo "Inserting new release" $ object ["new_release" .= newRelease]
      insertRelease newRelease
      env <- Reader.ask
      let instanceInfo =
            case env.environment of
              Production -> Right env.domain
              _ -> Left (env.domain, env.httpPort)
      entry <- Types.newReleaseEntry instanceInfo package newRelease.version
      Update.insertFeedEntry entry

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

updateTarballRootHash :: DB :> es => ReleaseId -> Sha256Sum -> Eff es ()
updateTarballRootHash releaseId hash =
  dbtToEff $
    void $
      updateFieldsBy @Release
        [[field| tarball_root_hash |]]
        ([field| release_id |], releaseId)
        (Only $ Just $ display hash)

updateTestedWith
  :: DB :> es
  => ReleaseId
  -> Vector Version
  -> UTCTime
  -> Eff es ()
updateTestedWith releaseId testedCompilers timestamp =
  dbtToEff $
    void $
      updateFieldsBy @Release
        [[field| tested_with |], [field| updated_at |]]
        ([field| release_id |], releaseId)
        (Just testedCompilers, timestamp)

updateTarballArchiveHash
  :: (BlobStoreAPI :> es, DB :> es)
  => ReleaseId
  -> LazyByteString
  -> Eff es ()
updateTarballArchiveHash releaseId (toStrict -> content) = do
  let hash = Sha256Sum . SHA.hash $ content
  put hash content
  dbtToEff $
    void $
      updateFieldsBy @Release
        [[field| tarball_archive_hash |]]
        ([field| release_id |], releaseId)
        (Only . Just $ display hash)

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

setArchiveChecksum :: DB :> es => ReleaseId -> Text -> Eff es ()
setArchiveChecksum releaseId sha256Hash =
  dbtToEff $
    void $
      updateFieldsBy @Release
        [[field| archive_checksum |]]
        ([field| release_id |], releaseId)
        (Only sha256Hash)
