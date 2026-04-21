{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageUploader.Query where

import Data.Text
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Entity.Internal.QQ (field)
import Database.PostgreSQL.Simple (Only (Only))
import Effectful
import Effectful.PostgreSQL.Transact.Effect

import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageUploader.Types

getPackageUploaderById
  :: DB :> es
  => PackageUploaderId
  -> Eff es (Maybe PackageUploader)
getPackageUploaderById packageUploaderId = do
  mDao <- dbtToEff $ selectById @PackageUploaderDAO (Only packageUploaderId)
  case mDao of
    Nothing -> pure Nothing
    Just dao -> do
      mPackageIndex <- Query.getPackageIndexById dao.packageIndexId
      case mPackageIndex of
        Nothing -> pure Nothing
        Just packageIndex ->
          pure $
            Just $
              PackageUploader
                { packageUploaderId = dao.packageUploaderId
                , username = dao.username
                , packageIndex = packageIndex
                , userId = dao.userId
                }

getPackageUploaderByUsernameAndIndex
  :: DB :> es
  => Text
  -> PackageIndexId
  -> Eff es (Maybe PackageUploader)
getPackageUploaderByUsernameAndIndex username packageIndexId = do
  mDao :: Maybe PackageUploaderDAO <- dbtToEff $ DBT.queryOne q (username, packageIndexId)
  case mDao of
    Nothing -> pure Nothing
    Just dao -> do
      mPackageIndex <- Query.getPackageIndexById dao.packageIndexId
      case mPackageIndex of
        Nothing -> pure Nothing
        Just packageIndex ->
          pure $
            Just $
              PackageUploader
                { packageUploaderId = dao.packageUploaderId
                , username = dao.username
                , packageIndex = packageIndex
                , userId = dao.userId
                }
  where
    q =
      _selectWhere @PackageUploaderDAO
        [ [field| username |]
        , [field| package_index_id |]
        ]
