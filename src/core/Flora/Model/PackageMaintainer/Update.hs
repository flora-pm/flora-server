module Flora.Model.PackageMaintainer.Update
  ( insertPackageMaintainers
  ) where

import Data.List (List)
import Database.PostgreSQL.Entity (insertMany)
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Flora.Model.PackageMaintainer.Types

insertPackageMaintainers
  :: DB :> es
  => List PackageMaintainer
  -> Eff es ()
insertPackageMaintainers packageMaintainers =
  dbtToEff $
    insertMany @PackageMaintainer packageMaintainers
