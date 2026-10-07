{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageGroup.Query
  ( getPackageGroupByPackageGroupName
  , listPackageGroups
  , getPackageGroupById
  ) where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (..))
import Effectful

import Flora.Database
import Flora.Model.PackageGroup.Types

getPackageGroupById :: (IOE :> es, ReadDB :> es) => PackageGroupId -> Eff es (Maybe PackageGroup)
getPackageGroupById groupId = queryOne (_selectWhere @PackageGroup [[field| package_group_id |]]) (Only groupId)

getPackageGroupByPackageGroupName :: (IOE :> es, ReadDB :> es) => PackageGroupName -> Eff es (Maybe PackageGroup)
getPackageGroupByPackageGroupName groupName = queryOne (_selectWhere @PackageGroup [[field| group_name |]]) (Only groupName)

listPackageGroups :: (IOE :> es, ReadDB :> es) => Eff es (Vector PackageGroup)
listPackageGroups =
  Vector.fromList <$> query_ (_select @PackageGroup <> _orderByMany [([field| group_name |], ASC)])
