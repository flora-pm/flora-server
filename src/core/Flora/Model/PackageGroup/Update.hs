module Flora.Model.PackageGroup.Update
  ( insertPackageGroup
  , addPackageToPackageGroup
  , removePackageFromPackageGroup
  ) where

import Control.Monad (void)
import Database.PostgreSQL.Entity (delete, insert)
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Flora.Model.PackageGroup.Types

insertPackageGroup :: DB :> es => PackageGroup -> Eff es ()
insertPackageGroup packageGroup = do
  void $ dbtToEff $ insert @PackageGroup packageGroup

addPackageToPackageGroup :: DB :> es => PackageGroupPackage -> Eff es ()
addPackageToPackageGroup packageGroupPackage =
  void $ dbtToEff $ insert @PackageGroupPackage packageGroupPackage

removePackageFromPackageGroup :: DB :> es => PackageGroupPackage -> Eff es ()
removePackageFromPackageGroup packageGroupPackage =
  void $ dbtToEff $ delete @PackageGroupPackage packageGroupPackage
