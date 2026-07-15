{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Release.Update
  ( insertRelease
  , upsertRelease
  , refreshLatestVersions
  , updateReadme
  , updateUploadTime
  , updateRevisionTime
  , updateTarballRootHash
  , updateChangelog
  , updateTarballArchiveHash
  , updateReleaseUploader
  , setReleasesDeprecationMarker
  , setArchiveChecksum
  , linkPackageUploaderToImportedRelease
  ) where

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
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToRow
import Distribution.Types.Version (Version)
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time (Time)
import Log qualified

import Flora.Database
import Flora.Domain.Import.Types
import Flora.Environment.Env (DeploymentEnv (..), FloraEnv (..))
import Flora.Model.BlobStore.API (BlobStoreAPI, put)
import Flora.Model.BlobStore.Types
import Flora.Model.Feed.Types qualified as Types
import Flora.Model.Feed.Update qualified as Update
import Flora.Model.Package.Types (Package (..))
import Flora.Model.PackageUploader.Query qualified as Query
import Flora.Model.PackageUploader.Types
import Flora.Model.PackageUploader.Update qualified as Update
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Monad

insertRelease :: (IOE :> es, WriteDB :> es) => Release -> FloraM es ()
insertRelease r = void $ execute (_insert @Release) r

upsertRelease
  :: (IOE :> es, Log :> es, ReadDB :> es, Reader FloraEnv :> es, Time :> es, WriteDB :> es)
  => Package -> Release -> FloraM es ()
upsertRelease package newRelease = do
  mReleaseFromDB <- Query.getReleaseById newRelease.releaseId
  case mReleaseFromDB of
    Just releaseFromDB ->
      when (releaseFromDB.testedWith == newRelease.testedWith) $ do
        Log.logInfo "Duplicate releases found" $
          object
            [ "new_release" .= newRelease
            , "release_from_db" .= releaseFromDB
            ]
        updateTestedWith
          newRelease.releaseId
          newRelease.testedWith
          newRelease.updatedAt
    Nothing -> do
      insertRelease newRelease
      env <- Reader.ask
      let instanceInfo =
            case env.environment of
              Production -> Right env.domain
              _ -> Left (env.domain, env.httpPort)
      entry <- Types.newReleaseEntry instanceInfo package newRelease.version
      Update.insertFeedEntry entry

refreshLatestVersions :: (IOE :> es, WriteDB :> es) => FloraM es ()
refreshLatestVersions = void $ execute [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "latest_versions" |] ()

updateReadme :: (IOE :> es, WriteDB :> es) => ReleaseId -> Maybe TextHtml -> ImportStatus -> FloraM es ()
updateReadme releaseId readmeBody status =
  void $
    execute
      ( _updateFieldsBy @Release
          [ [field| readme |]
          , [field| readme_status |]
          ]
          [field| release_id |]
      )
      (toRow (readmeBody, status) ++ toRow (Only releaseId))

updateUploadTime :: (IOE :> es, WriteDB :> es) => ReleaseId -> UTCTime -> FloraM es ()
updateUploadTime releaseId timestamp =
  void $
    execute
      ( _updateFieldsBy @Release
          [[field| uploaded_at |]]
          [field| release_id |]
      )
      (toRow (Only (Just timestamp)) ++ toRow (Only releaseId))

updateRevisionTime :: (IOE :> es, WriteDB :> es) => ReleaseId -> UTCTime -> FloraM es ()
updateRevisionTime releaseId timestamp =
  void $
    execute
      ( _updateFieldsBy @Release
          [[field| revised_at |]]
          [field| release_id |]
      )
      (toRow (Only (Just timestamp)) ++ toRow (Only releaseId))

updateChangelog :: (IOE :> es, WriteDB :> es) => ReleaseId -> Maybe TextHtml -> ImportStatus -> FloraM es ()
updateChangelog releaseId changelogBody status =
  void $
    execute
      ( _updateFieldsBy @Release
          [ [field| changelog |]
          , [field| changelog_status |]
          ]
          [field| release_id |]
      )
      (toRow (changelogBody, status) ++ toRow (Only releaseId))

updateTarballRootHash :: (IOE :> es, WriteDB :> es) => ReleaseId -> Sha256Sum -> FloraM es ()
updateTarballRootHash releaseId hash =
  void $
    execute
      ( _updateFieldsBy @Release
          [[field| tarball_root_hash |]]
          [field| release_id |]
      )
      (toRow (Only $ Just $ display hash) ++ toRow (Only releaseId))

updateTestedWith
  :: (IOE :> es, WriteDB :> es)
  => ReleaseId
  -> Vector Version
  -> UTCTime
  -> FloraM es ()
updateTestedWith releaseId testedCompilers timestamp =
  void $
    execute
      ( _updateFieldsBy @Release
          [[field| tested_with |], [field| updated_at |]]
          [field| release_id |]
      )
      (toRow (Just testedCompilers, timestamp) ++ toRow (Only releaseId))

updateTarballArchiveHash
  :: (BlobStoreAPI :> es, IOE :> es, WriteDB :> es)
  => ReleaseId
  -> LazyByteString
  -> FloraM es ()
updateTarballArchiveHash releaseId (toStrict -> content) = do
  let hash = Sha256Sum . SHA.hash $ content
  put hash content
  void $
    execute
      ( _updateFieldsBy @Release
          [[field| tarball_archive_hash |]]
          [field| release_id |]
      )
      (toRow (Only . Just $ display hash) ++ toRow (Only releaseId))

linkPackageUploaderToImportedRelease
  :: (Error ImportError :> es, IOE :> es, ReadDB :> es, WriteDB :> es)
  => ReleaseId
  -> Text
  -> FloraM es ()
linkPackageUploaderToImportedRelease releaseId username = do
  mPackageIndexId <- Query.getReleasePackageIndex releaseId
  case mPackageIndexId of
    Nothing -> Error.throwError $ CouldNotFindPackageIndexForRelease releaseId
    Just packageIndexId -> do
      mPackageUploader <-
        Query.getPackageUploaderByUsernameAndIndex
          username
          packageIndexId
      case mPackageUploader of
        Just packageUploader ->
          updateReleaseUploader releaseId packageUploader.packageUploaderId
        Nothing -> do
          packageUploaderDAO <- mkPackageUploaderDAO username packageIndexId Nothing
          Update.insertPackageUploader packageUploaderDAO
          updateReleaseUploader releaseId packageUploaderDAO.packageUploaderId

updateReleaseUploader
  :: (IOE :> es, WriteDB :> es)
  => ReleaseId
  -> PackageUploaderId
  -> FloraM es ()
updateReleaseUploader releaseId packageUploaderId =
  void $
    execute
      ( _updateFieldsBy @Release
          [[field| uploader_id |]]
          [field| release_id |]
      )
      (toRow (Only packageUploaderId) ++ toRow (Only releaseId))

setReleasesDeprecationMarker
  :: (IOE :> es, WriteDB :> es)
  => Vector (Bool, ReleaseId)
  -> FloraM es ()
setReleasesDeprecationMarker releaseVersions =
  void $ executeMany q (releaseVersions & Vector.toList)
  where
    q =
      [sql|
    UPDATE releases as r0
    SET deprecated = upd.x
    FROM (VALUES (?,?)) as upd(x,y)
    WHERE r0.release_id = (upd.y :: uuid)
    |]

setArchiveChecksum :: (IOE :> es, WriteDB :> es) => ReleaseId -> Text -> FloraM es ()
setArchiveChecksum releaseId sha256Hash =
  void $
    execute
      ( _updateFieldsBy @Release
          [[field| archive_checksum |]]
          [field| release_id |]
      )
      (toRow (Only sha256Hash) ++ toRow (Only releaseId))
