module Advisories.Model.Advisory.Update where

import Control.Monad
import Database.PostgreSQL.Entity
import Effectful

import Advisories.Model.Advisory.Types
import Flora.Database

insertAdvisory :: (IOE :> es, WriteDB :> es) => AdvisoryDAO -> Eff es ()
insertAdvisory = void . execute (_insert @AdvisoryDAO)
