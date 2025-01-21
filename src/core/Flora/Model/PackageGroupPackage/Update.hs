{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageGroupPackage.Update
  ( addPackageToPackageGroup
  , removePackageFromPackageGroup
  ) where

import Control.Monad (void)
import Database.PostgreSQL.Entity (deleteByField, insert)
import Database.PostgreSQL.Entity.Internal.QQ
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Flora.Model.Package.Types (PackageId (..))
import Flora.Model.PackageGroup.Types (PackageGroupId (..))
import Flora.Model.PackageGroupPackage.Types

addPackageToPackageGroup :: DB :> es => PackageGroupPackage -> Eff es ()
addPackageToPackageGroup packageGroupPackage =
  void $ dbtToEff $ insert @PackageGroupPackage packageGroupPackage

removePackageFromPackageGroup :: DB :> es => PackageId -> PackageGroupId -> Eff es ()
removePackageFromPackageGroup pId pgId =
  void $ dbtToEff $ deleteByField @PackageGroupPackage [[field|  package_id |], [field|  package_group_id |]] (pId, pgId)
