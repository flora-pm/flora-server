{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageGroupPackage.Update
  ( addPackageToPackageGroup
  , removePackageFromPackageGroup
  ) where

import Control.Monad (void)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Internal.QQ
import Effectful

import Flora.Database
import Flora.Model.Package.Types (PackageId (..))
import Flora.Model.PackageGroup.Types (PackageGroupId (..))
import Flora.Model.PackageGroupPackage.Types

addPackageToPackageGroup :: (IOE :> es, WriteDB :> es) => PackageGroupPackage -> Eff es ()
addPackageToPackageGroup packageGroupPackage =
  void $ execute (_insert @PackageGroupPackage) packageGroupPackage

removePackageFromPackageGroup :: (IOE :> es, WriteDB :> es) => PackageId -> PackageGroupId -> Eff es ()
removePackageFromPackageGroup pId pgId =
  void $ execute (_deleteWhere @PackageGroupPackage [[field|  package_id |], [field|  package_group_id |]]) (pId, pgId)
