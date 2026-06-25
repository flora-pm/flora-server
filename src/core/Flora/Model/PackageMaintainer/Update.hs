module Flora.Model.PackageMaintainer.Update
  ( insertPackageMaintainers
  ) where

import Control.Monad
import Data.List (List)
import Database.PostgreSQL.Entity
import Effectful

import Flora.Database
import Flora.Model.PackageMaintainer.Types

insertPackageMaintainers
  :: (IOE :> es, WriteDB :> es)
  => List PackageMaintainer
  -> Eff es ()
insertPackageMaintainers packageMaintainers =
  void $
    executeMany (_insert @PackageMaintainer) packageMaintainers
