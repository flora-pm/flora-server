{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageUploader.Update
  ( insertPackageUploader
  , getOrInsertPackageUploader
  , insertMaybeExistingPackageUploader
  ) where

import Control.Monad
import Data.Text (Text)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL

import Flora.Database
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageUploader.Query qualified as Query
import Flora.Model.PackageUploader.Types

insertPackageUploader
  :: (IOE :> es, Labeled ReadWrite WithConnection :> es)
  => PackageUploaderDAO
  -> Eff es ()
insertPackageUploader packageUploader =
  labeled @ReadWrite @WithConnection $
    void $
      execute (_insert @PackageUploaderDAO) packageUploader

insertMaybeExistingPackageUploader
  :: (IOE :> es, Labeled ReadWrite WithConnection :> es)
  => PackageUploaderDAO
  -> Eff es ()
insertMaybeExistingPackageUploader packageUploaderDAO = labeled @ReadWrite @WithConnection $ do
  void $ execute sqlQuery packageUploaderDAO
  where
    sqlQuery =
      [sql|
        INSERT INTO "package_uploaders"
        VALUES (?, ?, ?, ?)
        ON CONFLICT (username, package_index_id) DO NOTHING
      |]

getOrInsertPackageUploader
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es, Labeled ReadWrite WithConnection :> es)
  => Text
  -> PackageIndexId
  -> Eff es PackageUploaderId
getOrInsertPackageUploader username packageIndexId =
  Query.getPackageUploaderByUsernameAndIndex username packageIndexId >>= \case
    Just pu -> pure pu.packageUploaderId
    Nothing -> do
      packageUploaderDAO <-
        mkPackageUploaderDAO username packageIndexId Nothing
      insertPackageUploader packageUploaderDAO
      pure packageUploaderDAO.packageUploaderId
