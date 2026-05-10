{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageUploader.Query
  ( getPackageUploaderById
  , getPackageUploaderByUsernameAndIndex
  , getRecentlyActiveUploaders
  , getPackageUploaders
  ) where

import Data.Text
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (query)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Entity.Internal.QQ (field)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ
import Effectful
import Effectful.PostgreSQL.Transact.Effect

import Flora.Model.Package.Types
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageUploader.Types
import Flora.Monad

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

getRecentlyActiveUploaders
  :: DB :> es
  => FloraM es (Vector Text)
getRecentlyActiveUploaders = dbtToEff $ do
  result <- query sqlQuery ()
  pure $ fromOnly <$> result
  where
    sqlQuery =
      [sql|
      SELECT p0.username
      FROM package_uploaders AS p0
           INNER JOIN releases AS r1 ON p0.package_uploader_id = r1.uploader_id
      WHERE r1.uploaded_at >= (CURRENT_DATE - INTERVAL '2 years')
        AND r1.uploaded_at < CURRENT_DATE
      GROUP BY p0.username
      |]

getPackageUploaders
  :: DB :> es
  => PackageId
  -> FloraM es (Vector PackageUploaderDAO)
getPackageUploaders packageId = dbtToEff $ do
  query sqlQuery (Only packageId)
  where
    sqlQuery =
      [sql|
        SELECT p0.*
        FROM package_uploaders AS p0
             INNER JOIN releases AS r1 ON p0.package_uploader_id = r1.uploader_id
        WHERE r1.package_id = ?
      |]
