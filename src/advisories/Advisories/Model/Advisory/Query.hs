{-# LANGUAGE QuasiQuotes #-}

module Advisories.Model.Advisory.Query where

import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (Only))
import Effectful
import Effectful.PostgreSQL.Transact.Effect
import Security.Advisories.Core.HsecId

import Advisories.Model.Advisory.Types
import Advisories.Model.Affected.Types
import Flora.Model.Package.Types

getAdvisoryById :: DB :> es => AdvisoryId -> Eff es (Maybe AdvisoryDAO)
getAdvisoryById advisoryId = dbtToEff $ selectById (Only advisoryId)

getAdvisoryByHsecId :: DB :> es => HsecId -> Eff es (Maybe AdvisoryDAO)
getAdvisoryByHsecId hsecId = dbtToEff $ selectOneByField [field| hsec_id |] (Only hsecId)

getAdvisoriesByPackageId
  :: DB :> es
  => PackageId
  -> Eff es (Vector AdvisoryDAO)
getAdvisoriesByPackageId packageId =
  dbtToEff $
    joinSelectOneByField @AdvisoryDAO @AffectedPackageDAO
      [field| advisory_id |]
      [field| package_id |]
      packageId
