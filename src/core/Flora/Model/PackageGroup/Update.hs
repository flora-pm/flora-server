module Flora.Model.PackageGroup.Update
  ( insertPackageGroup
  ) where

import Control.Monad (void)
import Database.PostgreSQL.Entity (insert)
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Flora.Model.PackageGroup.Types

insertPackageGroup :: DB :> es => PackageGroup -> Eff es ()
insertPackageGroup packageGroup = do
  void $ dbtToEff $ insert @PackageGroup packageGroup
