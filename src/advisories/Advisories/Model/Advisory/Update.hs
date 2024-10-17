module Advisories.Model.Advisory.Update where

import Database.PostgreSQL.Entity (insert)
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Advisories.Model.Advisory.Types

insertAdvisory :: DB :> es => AdvisoryDAO -> Eff es ()
insertAdvisory = dbtToEff . insert @AdvisoryDAO
