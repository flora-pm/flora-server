module Flora.Model.PackageMaintainer.Update
  ( insertPackageMaintainers
  ) where

import Control.Monad
import Data.List (List)
import Database.PostgreSQL.Entity
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL

import Flora.Database
import Flora.Model.PackageMaintainer.Types

insertPackageMaintainers
  :: (IOE :> es, Labeled ReadWrite WithConnection :> es)
  => List PackageMaintainer
  -> Eff es ()
insertPackageMaintainers packageMaintainers =
  labeled @ReadWrite @WithConnection $
    void $
      executeMany (_insert @PackageMaintainer) packageMaintainers
