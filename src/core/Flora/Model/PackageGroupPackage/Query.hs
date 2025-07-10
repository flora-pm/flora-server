{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageGroupPackage.Query
  ( getPackageGroupPackage
  , listPackageGroupPackages
  , getPackageGroupsForPackage
  ) where

import Data.Vector (Vector)
import Database.PostgreSQL.Entity (selectById)
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Flora.Model.Package.Types
import Flora.Model.PackageGroup.Types
import Flora.Model.PackageGroupPackage.Types

getPackageGroupPackage :: DB :> es => PackageGroupPackageId -> Eff es (Maybe PackageGroupPackage)
getPackageGroupPackage packageGroupPackageId = dbtToEff $ selectById @PackageGroupPackage (Only packageGroupPackageId)

getPackageGroupsForPackage :: DB :> es => PackageId -> Eff es (Vector PackageGroupName)
getPackageGroupsForPackage packageId = do
  results :: Vector (Only PackageGroupName) <- dbtToEff $ query q (Only packageId)
  pure $ fmap fromOnly results
  where
    q =
      [sql|
        SELECT p1.group_name
        FROM package_group_packages AS p0
          INNER JOIN package_groups AS p1 ON p0.package_group_id = p1.package_group_id
        WHERE p0.package_id = ?
      |]

listPackageGroupPackages :: DB :> es => PackageGroupId -> Eff es (Vector PackageInfo)
listPackageGroupPackages groupId = dbtToEff $ query q (Only groupId)
  where
    q =
      [sql|
SELECT lv.package_id
     , lv.namespace
     , lv.name
     , lv.synopsis
     , lv.version
     , lv.license
     , 1
     , lv.uploaded_at
     , lv.revised_at
FROM package_group_packages AS p1
     INNER JOIN latest_versions AS lv
         ON p1.package_group_id = ?
        AND p1.package_id = lv.package_id
    |]
