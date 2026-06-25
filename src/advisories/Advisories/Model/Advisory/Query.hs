{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Advisories.Model.Advisory.Query where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (Only))
import Effectful
import Security.Advisories.Core.HsecId

import Advisories.Model.Advisory.Types
import Advisories.Model.Affected.Types
import Flora.Database
import Flora.Model.Package.Types

getAdvisoryById :: (IOE :> es, ReadDB :> es) => AdvisoryId -> Eff es (Maybe AdvisoryDAO)
getAdvisoryById advisoryId = queryOne (_selectWhere @AdvisoryDAO [primaryKey @AdvisoryDAO]) (Only advisoryId)

getAdvisoryByHsecId :: (IOE :> es, ReadDB :> es) => HsecId -> Eff es (Maybe AdvisoryDAO)
getAdvisoryByHsecId hsecId = queryOne (_selectWhere @AdvisoryDAO [[field| hsec_id |]]) (Only hsecId)

getAdvisoriesByPackageId
  :: (IOE :> es, ReadDB :> es)
  => PackageId
  -> Eff es (Vector AdvisoryDAO)
getAdvisoriesByPackageId packageId =
  Vector.fromList
    <$> query (_joinSelectOneByField @AdvisoryDAO @AffectedPackageDAO [field| advisory_id |] [field| package_id |]) (Only packageId)
