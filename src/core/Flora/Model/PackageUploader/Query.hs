{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageUploader.Query
  ( getPackageUploaderById
  , getPackageUploaderByUsernameAndIndex
  , getPackageUploaders
  ) where

import Data.Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Internal.QQ (field)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL

import Flora.Database
import Flora.Model.Package.Types
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageUploader.Types
import Flora.Monad

getPackageUploaderById
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => PackageUploaderId
  -> Eff es (Maybe PackageUploader)
getPackageUploaderById packageUploaderId = do
  mDao :: Maybe PackageUploaderDAO <- labeled @ReadOnly @WithConnection $ queryOne (_selectWhere @PackageUploaderDAO [primaryKey @PackageUploaderDAO]) (Only packageUploaderId)
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
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => Text
  -> PackageIndexId
  -> Eff es (Maybe PackageUploader)
getPackageUploaderByUsernameAndIndex username packageIndexId = do
  mDao :: Maybe PackageUploaderDAO <- labeled @ReadOnly @WithConnection $ queryOne q (username, packageIndexId)
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

getPackageUploaders
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => PackageId
  -> FloraM es (Vector PackageUploaderDAO)
getPackageUploaders packageId =
  labeled @ReadOnly @WithConnection $
    Vector.fromList
      <$> query sqlQuery (Only packageId)
  where
    sqlQuery =
      [sql|
        SELECT p0.*
        FROM package_uploaders AS p0
             INNER JOIN releases AS r1 ON p0.package_uploader_id = r1.uploader_id
        WHERE r1.package_id = ?
      |]
