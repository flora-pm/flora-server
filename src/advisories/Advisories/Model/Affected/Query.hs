{-# LANGUAGE QuasiQuotes #-}

module Advisories.Model.Affected.Query where

import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Security.Advisories.Core.HsecId

import Advisories.HsecId.Orphans ()
import Advisories.Model.Advisory.Types
import Advisories.Model.Affected.Types

getAffectedPackageById
  :: DB :> es
  => AffectedPackageId
  -> Eff es (Maybe AffectedPackageDAO)
getAffectedPackageById affectedPackageId = dbtToEff $ selectById (Only affectedPackageId)

getAffectedPackagesByAdvisoryId
  :: DB :> es
  => AdvisoryId
  -> Eff es (Vector AffectedPackageDAO)
getAffectedPackagesByAdvisoryId advisoryId =
  dbtToEff $ selectManyByField @AffectedPackageDAO [field| advisory_id |] (Only advisoryId)

getAffectedPackagesByHsecId
  :: DB :> es
  => HsecId
  -> Eff es (Vector AffectedPackageDAO)
getAffectedPackagesByHsecId hsecId =
  dbtToEff $
    joinSelectOneByField @AffectedPackageDAO @AdvisoryDAO
      [field| advisory_id |]
      [field| hsec_id |]
      hsecId
