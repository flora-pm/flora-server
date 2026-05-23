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
import Effectful.Labeled
import Effectful.PostgreSQL

import Flora.Database
import Flora.Model.PackageGroup.Types

getPackageGroupById :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => PackageGroupId -> Eff es (Maybe PackageGroup)
getPackageGroupById groupId = labeled @_ @WithConnection $ queryOne (_selectWhere @PackageGroup [[field| package_group_id |]]) (Only groupId)

getPackageGroupByPackageGroupName :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => PackageGroupName -> Eff es (Maybe PackageGroup)
getPackageGroupByPackageGroupName groupName = labeled @_ @WithConnection $ queryOne (_selectWhere @PackageGroup [[field| group_name |]]) (Only groupName)

listPackageGroups :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => Eff es (Vector PackageGroup)
listPackageGroups =
  labeled @_ @WithConnection $ Vector.fromList <$> query_ (_select @PackageGroup <> _orderByMany [([field| group_name |], ASC)])
