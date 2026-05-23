module Flora.Model.PackageGroup.Update
  ( insertPackageGroup
  , deletePackageGroup
  ) where

import Control.Monad (void)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple.Types
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL

import Flora.Database
import Flora.Model.PackageGroup.Types

insertPackageGroup :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => PackageGroup -> Eff es ()
insertPackageGroup packageGroup = do
  void $ labeled @ReadWrite @WithConnection $ execute (_insert @PackageGroup) packageGroup

deletePackageGroup :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => PackageGroupId -> Eff es ()
deletePackageGroup packageGroupId = do
  void $ labeled @ReadWrite @WithConnection $ execute (_delete @PackageGroup) (Only packageGroupId)
