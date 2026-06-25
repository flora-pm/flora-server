module Advisories.Model.Affected.Update where

import Control.Monad
import Database.PostgreSQL.Entity
import Effectful

import Advisories.Model.Affected.Types
import Flora.Database

insertAffectedPackage
  :: (IOE :> es, WriteDB :> es)
  => AffectedPackageDAO
  -> Eff es ()
insertAffectedPackage = void . execute (_insert @AffectedPackageDAO)

insertAffectedVersionRange
  :: (IOE :> es, WriteDB :> es)
  => AffectedVersionRangeDAO
  -> Eff es ()
insertAffectedVersionRange = void . execute (_insert @AffectedVersionRangeDAO)
