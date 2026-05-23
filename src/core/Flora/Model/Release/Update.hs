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
import Effectful.Labeled
import Effectful.Log (Log)
import Effectful.PostgreSQL
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time (Time)
import Log qualified

import Flora.Database
import Flora.Environment.Env (DeploymentEnv (..), FloraEnv (..))
import Flora.Import.Types
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

insertRelease :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => Release -> FloraM es ()
insertRelease r = labeled @ReadWrite @WithConnection $ void $ execute (_insert @Release) r

upsertRelease
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es, Labeled ReadWrite WithConnection :> es, Log :> es, Reader FloraEnv :> es, Time :> es)
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

refreshLatestVersions :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => FloraM es ()
refreshLatestVersions = labeled @ReadWrite @WithConnection $ void $ execute [sql| REFRESH MATERIALIZED VIEW CONCURRENTLY "latest_versions" |] ()

updateReadme :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => ReleaseId -> Maybe TextHtml -> ImportStatus -> FloraM es ()
updateReadme releaseId readmeBody status =
  labeled @ReadWrite @WithConnection $
    void $
      execute
        ( _updateFieldsBy @Release
            [ [field| readme |]
            , [field| readme_status |]
            ]
            [field| release_id |]
        )
        (toRow (readmeBody, status) ++ toRow (Only releaseId))

updateUploadTime :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => ReleaseId -> UTCTime -> FloraM es ()
updateUploadTime releaseId timestamp =
  labeled @ReadWrite @WithConnection $
    void $
      execute
        ( _updateFieldsBy @Release
            [[field| uploaded_at |]]
            [field| release_id |]
        )
        (toRow (Only (Just timestamp)) ++ toRow (Only releaseId))

updateRevisionTime :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => ReleaseId -> UTCTime -> FloraM es ()
updateRevisionTime releaseId timestamp =
  labeled @ReadWrite @WithConnection $
    void $
      execute
        ( _updateFieldsBy @Release
            [[field| revised_at |]]
            [field| release_id |]
        )
        (toRow (Only (Just timestamp)) ++ toRow (Only releaseId))

updateChangelog :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => ReleaseId -> Maybe TextHtml -> ImportStatus -> FloraM es ()
updateChangelog releaseId changelogBody status =
  labeled @ReadWrite @WithConnection $
    void $
      execute
        ( _updateFieldsBy @Release
            [ [field| changelog |]
            , [field| changelog_status |]
            ]
            [field| release_id |]
        )
        (toRow (changelogBody, status) ++ toRow (Only releaseId))

updateTarballRootHash :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => ReleaseId -> Sha256Sum -> FloraM es ()
updateTarballRootHash releaseId hash =
  labeled @ReadWrite @WithConnection $
    void $
      execute
        ( _updateFieldsBy @Release
            [[field| tarball_root_hash |]]
            [field| release_id |]
        )
        (toRow (Only $ Just $ display hash) ++ toRow (Only releaseId))

updateTestedWith
  :: (IOE :> es, Labeled ReadWrite WithConnection :> es)
  => ReleaseId
  -> Vector Version
  -> UTCTime
  -> FloraM es ()
updateTestedWith releaseId testedCompilers timestamp =
  labeled @ReadWrite @WithConnection $
    void $
      execute
        ( _updateFieldsBy @Release
            [[field| tested_with |], [field| updated_at |]]
            [field| release_id |]
        )
        (toRow (Just testedCompilers, timestamp) ++ toRow (Only releaseId))

updateTarballArchiveHash
  :: (BlobStoreAPI :> es, IOE :> es, Labeled ReadWrite WithConnection :> es)
  => ReleaseId
  -> LazyByteString
  -> FloraM es ()
updateTarballArchiveHash releaseId (toStrict -> content) = do
  let hash = Sha256Sum . SHA.hash $ content
  put hash content
  labeled @ReadWrite @WithConnection $
    void $
      execute
        ( _updateFieldsBy @Release
            [[field| tarball_archive_hash |]]
            [field| release_id |]
        )
        (toRow (Only . Just $ display hash) ++ toRow (Only releaseId))

linkPackageUploaderToImportedRelease
  :: (Error ImportError :> es, IOE :> es, Labeled ReadOnly WithConnection :> es, Labeled ReadWrite WithConnection :> es, Reader FloraEnv :> es)
  => ReleaseId
  -> Text
  -> FloraM es ()
linkPackageUploaderToImportedRelease releaseId username = do
  FloraEnv{pool} <- Reader.ask
  mPackageIndexId <- Query.getReleasePackageIndex releaseId
  case mPackageIndexId of
    Nothing -> Error.throwError $ CouldNotFindPackageIndexForRelease releaseId
    Just packageIndexId -> do
      mPackageUploader <-
        withReadOnlyPool pool $
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
  :: (IOE :> es, Labeled ReadWrite WithConnection :> es)
  => ReleaseId
  -> PackageUploaderId
  -> FloraM es ()
updateReleaseUploader releaseId packageUploaderId =
  labeled @ReadWrite @WithConnection $
    void $
      execute
        ( _updateFieldsBy @Release
            [[field| uploader_id |]]
            [field| release_id |]
        )
        (toRow (Only packageUploaderId) ++ toRow (Only releaseId))

setReleasesDeprecationMarker
  :: (IOE :> es, Labeled ReadWrite WithConnection :> es)
  => Vector (Bool, ReleaseId)
  -> FloraM es ()
setReleasesDeprecationMarker releaseVersions =
  labeled @ReadWrite @WithConnection $ void $ executeMany q (releaseVersions & Vector.toList)
  where
    q =
      [sql|
    UPDATE releases as r0
    SET deprecated = upd.x
    FROM (VALUES (?,?)) as upd(x,y)
    WHERE r0.release_id = (upd.y :: uuid)
    |]

setArchiveChecksum :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => ReleaseId -> Text -> FloraM es ()
setArchiveChecksum releaseId sha256Hash =
  labeled @ReadWrite @WithConnection $
    void $
      execute
        ( _updateFieldsBy @Release
            [[field| archive_checksum |]]
            [field| release_id |]
        )
        (toRow (Only sha256Hash) ++ toRow (Only releaseId))
