module Advisories.Model.Affected.Update where

import Control.Monad
import Database.PostgreSQL.Entity
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL

import Advisories.Model.Affected.Types
import Flora.Database

insertAffectedPackage
  :: (IOE :> es, Labeled ReadWrite WithConnection :> es)
  => AffectedPackageDAO
  -> Eff es ()
insertAffectedPackage = labeled @ReadWrite @WithConnection . void . execute (_insert @AffectedPackageDAO)

insertAffectedVersionRange
  :: (IOE :> es, Labeled ReadWrite WithConnection :> es)
  => AffectedVersionRangeDAO
  -> Eff es ()
insertAffectedVersionRange = labeled @ReadWrite @WithConnection . void . execute (_insert @AffectedVersionRangeDAO)
