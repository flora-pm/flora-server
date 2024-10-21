module Advisories.Model.Affected.Update where

import Database.PostgreSQL.Entity (insert)
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Advisories.Model.Affected.Types

insertAffectedPackage
  :: DB :> es
  => AffectedPackageDAO
  -> Eff es ()
insertAffectedPackage = dbtToEff . insert @AffectedPackageDAO

insertAffectedVersionRange
  :: DB :> es
  => AffectedVersionRangeDAO
  -> Eff es ()
insertAffectedVersionRange = dbtToEff . insert @AffectedVersionRangeDAO
