{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Advisories.Model.Advisory.Query where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (Only))
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL
import Security.Advisories.Core.HsecId

import Advisories.Model.Advisory.Types
import Advisories.Model.Affected.Types
import Flora.Database
import Flora.Model.Package.Types

getAdvisoryById :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => AdvisoryId -> Eff es (Maybe AdvisoryDAO)
getAdvisoryById advisoryId = labeled @ReadOnly @WithConnection $ queryOne (_selectWhere @AdvisoryDAO [primaryKey @AdvisoryDAO]) (Only advisoryId)

getAdvisoryByHsecId :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => HsecId -> Eff es (Maybe AdvisoryDAO)
getAdvisoryByHsecId hsecId = labeled @ReadOnly @WithConnection $ queryOne (_selectWhere @AdvisoryDAO [[field| hsec_id |]]) (Only hsecId)

getAdvisoriesByPackageId
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => PackageId
  -> Eff es (Vector AdvisoryDAO)
getAdvisoriesByPackageId packageId =
  labeled @ReadOnly @WithConnection $
    Vector.fromList
      <$> query (_joinSelectOneByField @AdvisoryDAO @AffectedPackageDAO [field| advisory_id |] [field| package_id |]) (Only packageId)
