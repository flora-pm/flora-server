{-# LANGUAGE QuasiQuotes #-}

module Advisories.Model.Advisory.Query where

import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (Only))
import Effectful
import Effectful.PostgreSQL.Transact.Effect

import Advisories.Model.Advisory.Types
import Flora.Model.User

getAdvisoryById :: DB :> es => UserId -> Eff es (Maybe AdvisoryDAO)
getAdvisoryById advisoryId = dbtToEff $ selectById (Only advisoryId)

getAdvisoryByHsecId :: DB :> es => UserId -> Eff es (Maybe AdvisoryDAO)
getAdvisoryByHsecId hsecId = dbtToEff $ selectOneByField [field| hsec_id |] (Only hsecId)
