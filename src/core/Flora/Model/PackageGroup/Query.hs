{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageGroup.Query
  ( getPackagesByPackageGroupId
  , getPackageGroupByPackageGroupName
  , listPackageGroups
  , getPackageGroupById
  ) where

import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (..))
import Effectful (Eff, type (:>))
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Flora.Model.Package.Types (Package)
import Flora.Model.PackageGroup.Types (PackageGroup (..), PackageGroupId (..), PackageGroupName)
import Flora.Model.PackageGroupPackage.Types (PackageGroupPackage (..))

getPackagesByPackageGroupId :: DB :> es => PackageGroupId -> Eff es (Vector Package)
getPackagesByPackageGroupId packageGroupId =
  dbtToEff $
    joinSelectOneByField
      @Package
      @PackageGroupPackage
      [field| package_id |]
      [field| package_group_id |]
      packageGroupId

getPackageGroupById :: DB :> es => PackageGroupId -> Eff es (Maybe PackageGroup)
getPackageGroupById groupId = dbtToEff $ selectOneByField [field| package_group_id |] (Only groupId)

getPackageGroupByPackageGroupName :: DB :> es => PackageGroupName -> Eff es (Maybe PackageGroup)
getPackageGroupByPackageGroupName groupName = dbtToEff $ selectOneByField [field| group_name |] (Only groupName)

listPackageGroups :: DB :> es => Eff es (Vector PackageGroup)
listPackageGroups =
  dbtToEff $ selectOrderBy @PackageGroup [([field| group_name |], ASC)]
