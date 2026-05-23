module Advisories.Model.Advisory.Update where

import Control.Monad
import Database.PostgreSQL.Entity
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL

import Advisories.Model.Advisory.Types
import Flora.Database

insertAdvisory :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => AdvisoryDAO -> Eff es ()
insertAdvisory = labeled @ReadWrite @WithConnection . void . execute (_insert @AdvisoryDAO)
