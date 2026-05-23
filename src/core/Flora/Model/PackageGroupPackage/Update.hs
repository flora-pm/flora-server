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
import Effectful.Labeled
import Effectful.PostgreSQL

import Flora.Database
import Flora.Model.Package.Types (PackageId (..))
import Flora.Model.PackageGroup.Types (PackageGroupId (..))
import Flora.Model.PackageGroupPackage.Types

addPackageToPackageGroup :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => PackageGroupPackage -> Eff es ()
addPackageToPackageGroup packageGroupPackage =
  void $ labeled @ReadWrite @WithConnection $ execute (_insert @PackageGroupPackage) packageGroupPackage

removePackageFromPackageGroup :: (IOE :> es, Labeled w0 WithConnection :> es) => PackageId -> PackageGroupId -> Eff es ()
removePackageFromPackageGroup pId pgId =
  void $ labeled @_ @WithConnection $ execute (_deleteWhere @PackageGroupPackage [[field|  package_id |], [field|  package_group_id |]]) (pId, pgId)
