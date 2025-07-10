module Flora.Model.PackageGroup.Update
  ( insertPackageGroup
  , deletePackageGroup
  ) where

import Control.Monad (void)
import Database.PostgreSQL.Entity (delete, insert)
import Database.PostgreSQL.Simple
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Flora.Model.PackageGroup.Types

insertPackageGroup :: DB :> es => PackageGroup -> Eff es ()
insertPackageGroup packageGroup = do
  void $ dbtToEff $ insert @PackageGroup packageGroup

deletePackageGroup :: DB :> es => PackageGroupId -> Eff es ()
deletePackageGroup packageGroupId = do
  void $ dbtToEff $ delete @PackageGroup (Only packageGroupId)
