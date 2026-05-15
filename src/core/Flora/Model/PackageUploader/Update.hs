module Flora.Model.PackageUploader.Update
  ( insertPackageUploader
  , getOrInsertPackageUploader
  ) where

import Data.Text (Text)
import Database.PostgreSQL.Entity (insert)
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Flora.Model.PackageIndex.Types
import Flora.Model.PackageUploader.Query qualified as Query
import Flora.Model.PackageUploader.Types

insertPackageUploader
  :: DB :> es
  => PackageUploaderDAO
  -> Eff es ()
insertPackageUploader packageUploader =
  dbtToEff $
    insert @PackageUploaderDAO packageUploader

getOrInsertPackageUploader
  :: (DB :> es, IOE :> es)
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
