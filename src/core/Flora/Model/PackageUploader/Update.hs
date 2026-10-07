{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageUploader.Update
  ( getOrInsertPackageUploader
  , insertMaybeExistingPackageUploader
  ) where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as Text
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Effectful.Exception qualified as E

import Flora.Database
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageUploader.Query qualified as Query
import Flora.Model.PackageUploader.Types

insertMaybeExistingPackageUploader
  :: (IOE :> es, WriteDB :> es)
  => PackageUploaderDAO
  -> Eff es ()
insertMaybeExistingPackageUploader packageUploaderDAO = do
  void $ execute sqlQuery packageUploaderDAO
  where
    sqlQuery =
      [sql|
        INSERT INTO "package_uploaders"
        VALUES (?, ?, ?, ?)
        ON CONFLICT (username, package_index_id) DO NOTHING
      |]

-- | The id of an index's uploader, inserting the row if this is the first time
-- the username is seen.
getOrInsertPackageUploader
  :: (IOE :> es, ReadDB :> es, WriteDB :> es)
  => Text
  -> PackageIndexId
  -> Eff es PackageUploaderId
getOrInsertPackageUploader username packageIndexId =
  lookupUploader >>= \case
    Just packageUploaderId -> pure packageUploaderId
    Nothing -> do
      packageUploaderDAO <- mkPackageUploaderDAO username packageIndexId Nothing
      insertMaybeExistingPackageUploader packageUploaderDAO
      lookupUploader >>= \case
        Just packageUploaderId -> pure packageUploaderId
        Nothing ->
          E.throwIO . userError $
            "no package_uploaders row for "
              <> Text.unpack username
              <> " after inserting one!"
  where
    lookupUploader =
      Query.getPackageUploaderIdByUsernameAndIndex username packageIndexId
