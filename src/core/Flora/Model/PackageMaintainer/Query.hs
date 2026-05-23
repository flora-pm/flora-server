{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageMaintainer.Query
  ( getPackageMaintainerById
  , getActiveMaintainers
  , getPackageMaintainers
  , getPackageMaintainerByUsernameAndIndex
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
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageMaintainer.Types
import Flora.Monad

getPackageMaintainerById
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => PackageMaintainerId
  -> Eff es (Maybe PackageMaintainer)
getPackageMaintainerById packageMaintainerId = do
  labeled @ReadOnly @WithConnection $ queryOne (_selectWhere @PackageMaintainer [primaryKey @PackageMaintainer]) (Only packageMaintainerId)

getPackageMaintainerByUsernameAndIndex
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => Text
  -> PackageIndexId
  -> Eff es (Maybe PackageMaintainer)
getPackageMaintainerByUsernameAndIndex username packageIndexId = do
  labeled @ReadOnly @WithConnection $ queryOne q (username, packageIndexId)
  where
    q =
      _selectWhere @PackageMaintainer
        [ [field| username |]
        , [field| package_index_id |]
        ]

getPackageMaintainers
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => PackageId
  -> Eff es (Vector PackageMaintainer)
getPackageMaintainers packageId =
  labeled @ReadOnly @WithConnection $ Vector.fromList <$> query (_selectWhere @PackageMaintainer [[field| package_id |]]) (Only packageId)

getActiveMaintainers
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => PackageId
  -> FloraM es (Vector Text)
getActiveMaintainers packageId = labeled @ReadOnly @WithConnection $ do
  result <- Vector.fromList <$> query sqlQuery (Only packageId)
  pure $ fromOnly <$> result
  where
    sqlQuery =
      [sql|
        SELECT p1.username
          FROM package_maintainers as p0
          INNER JOIN package_uploaders as p1 ON p0.package_uploader_id = p1.package_uploader_id
        WHERE p0.package_id = ?
          AND p1.username IN (SELECT p1.username
                              FROM package_maintainers AS p0
                                   INNER JOIN package_uploaders as p1 ON p0.package_uploader_id = p1.package_uploader_id
                                   INNER JOIN releases AS r2 ON p0.package_uploader_id = r2.uploader_id
                              WHERE r2.uploaded_at >= (CURRENT_DATE - CAST('2 years' AS interval))
                              GROUP BY p1.username)
        GROUP BY p1.package_uploader_id
      |]
