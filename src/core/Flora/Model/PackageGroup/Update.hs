module Flora.Model.PackageGroup.Update
  ( insertPackageGroup
  , deletePackageGroup
  ) where

import Control.Monad (void)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple.Types
import Effectful

import Flora.Database
import Flora.Model.PackageGroup.Types

insertPackageGroup :: (IOE :> es, WriteDB :> es) => PackageGroup -> Eff es ()
insertPackageGroup packageGroup = do
  void $ execute (_insert @PackageGroup) packageGroup

deletePackageGroup :: (IOE :> es, WriteDB :> es) => PackageGroupId -> Eff es ()
deletePackageGroup packageGroupId = do
  void $ execute (_delete @PackageGroup) (Only packageGroupId)
