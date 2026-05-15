{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageMaintainer.Query
  ( getPackageMaintainerById
  , getRecentlyActiveMaintainers
  , getPackageMaintainers
  , getPackageMaintainerByUsernameAndIndex
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
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageMaintainer.Types
import Flora.Monad

getPackageMaintainerById
  :: DB :> es
  => PackageMaintainerId
  -> Eff es (Maybe PackageMaintainer)
getPackageMaintainerById packageMaintainerId = do
  dbtToEff $ selectById @PackageMaintainer (Only packageMaintainerId)

getPackageMaintainerByUsernameAndIndex
  :: DB :> es
  => Text
  -> PackageIndexId
  -> Eff es (Maybe PackageMaintainer)
getPackageMaintainerByUsernameAndIndex username packageIndexId = do
  dbtToEff $ DBT.queryOne q (username, packageIndexId)
  where
    q =
      _selectWhere @PackageMaintainer
        [ [field| username |]
        , [field| package_index_id |]
        ]

getRecentlyActiveMaintainers
  :: DB :> es
  => FloraM es (Vector Text)
getRecentlyActiveMaintainers = dbtToEff $ do
  result <- query sqlQuery ()
  pure $ fromOnly <$> result
  where
    sqlQuery =
      [sql|
      SELECT p0.username
      FROM package_maintainers AS p0
           INNER JOIN releases AS r1 ON p0.package_uploader_id = r1.uploader_id
      WHERE r1.uploaded_at >= (CURRENT_DATE - INTERVAL '2 years')
        AND r1.uploaded_at < CURRENT_DATE
      GROUP BY p0.username
      |]

-- getPackageMaintainers
--   :: DB :> es
--   => PackageId
--   -> FloraM es (Vector PackageMaintainer)
-- getPackageMaintainers packageId = dbtToEff $ do
--   query sqlQuery (Only packageId)
--   where
--     sqlQuery =
--       [sql|
--         SELECT p0.*
--         FROM package_maintainers AS p0
--              INNER JOIN releases AS r1 ON p0.package_uploader_id = r1.uploader_id
--         WHERE r1.package_id = ?
--       |]

getPackageMaintainers
  :: DB :> es
  => PackageId
  -> Eff es (Vector PackageMaintainer)
getPackageMaintainers packageId =
  dbtToEff $
    selectManyByField @PackageMaintainer
      [field| package_id |]
      (Only packageId)
