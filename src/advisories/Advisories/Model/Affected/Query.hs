{-# LANGUAGE QuasiQuotes #-}

module Advisories.Model.Affected.Query where

import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Security.Advisories.Core.HsecId

import Advisories.HsecId.Orphans ()
import Advisories.Model.Advisory.Types
import Advisories.Model.Affected.Types
import Flora.Model.Package.Types

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

getAdvisoryPreviewsByPackageId :: DB :> es => PackageId -> Eff es (Vector PackageAdvisoryPreview)
getAdvisoryPreviewsByPackageId packageId =
  dbtToEff $
    query
      Select
      [sql|
SELECT s0.hsec_id
     , s0.summary
     , CASE
         WHEN a2.fixed_version IS NULL
           THEN FALSE
         ELSE TRUE
       END as fixed
     , s0.published
     , a1.cvss
FROM security_advisories AS s0
     INNER JOIN affected_packages AS a1 ON s0.advisory_id = a1.advisory_id
     INNER JOIN affected_version_ranges AS a2 ON a1.affected_package_id = a2.affected_package_id
     INNER JOIN packages AS p3 ON a1.package_id = p3.package_id
WHERE a1.package_id = ?
  |]
      (Only packageId)
