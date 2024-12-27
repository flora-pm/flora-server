{-# LANGUAGE QuasiQuotes #-}

module Advisories.Model.Affected.Query where

import Data.Text (Text)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query, queryOne)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..), Query)
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

searchInAdvisories :: DB :> es => (Word, Word) -> Text -> Eff es (Vector PackageAdvisoryPreview)
searchInAdvisories (offset, limit) searchTerm =
  dbtToEff $
    query
      Select
      searchAdvisoriesQuery
      (searchTerm, searchTerm, offset, limit)

searchAdvisoriesQuery :: Query
searchAdvisoriesQuery =
  [sql|
WITH results AS (
  SELECT s0.hsec_id
       , s0.summary
       , CASE
           WHEN a2.fixed_version IS NULL
             THEN FALSE
           ELSE TRUE
         END as fixed
       , s0.published
       , a1.cvss
       , word_similarity(s0.summary, ?) as rating
  FROM security_advisories AS s0
       INNER JOIN affected_packages AS a1 ON s0.advisory_id = a1.advisory_id
       INNER JOIN affected_version_ranges AS a2 ON a1.affected_package_id = a2.affected_package_id
       INNER JOIN packages AS p3 ON a1.package_id = p3.package_id
  WHERE ? <% s0.summary
  ORDER BY rating desc, s0.summary asc
  OFFSET ?
  LIMIT ?
)

SELECT r0.hsec_id
     , r0.summary
     , r0.fixed
     , r0.published
     , r0.cvss
FROM results as r0
  |]

countAdvisorySearchResults :: DB :> es => Text -> Eff es Word
countAdvisorySearchResults searchTerm =
  dbtToEff $ do
    (result :: Maybe (Only Int)) <-
      queryOne
        Select
        countAdvisorySearchResultsQuery
        (searchTerm, searchTerm)
    case result of
      Just (Only n) -> pure $ fromIntegral n
      Nothing -> pure 0

countAdvisorySearchResultsQuery :: Query
countAdvisorySearchResultsQuery =
  [sql|
WITH results AS (
  SELECT s0.hsec_id
       , s0.summary
       , CASE
           WHEN a2.fixed_version IS NULL
             THEN FALSE
           ELSE TRUE
         END as fixed
       , s0.published
       , a1.cvss
       , word_similarity(s0.summary, ?) as rating
  FROM security_advisories AS s0
       INNER JOIN affected_packages AS a1 ON s0.advisory_id = a1.advisory_id
       INNER JOIN affected_version_ranges AS a2 ON a1.affected_package_id = a2.affected_package_id
       INNER JOIN packages AS p3 ON a1.package_id = p3.package_id
  WHERE ? <% s0.summary
  ORDER BY rating desc, s0.summary asc
)

SELECT COUNT(*) FROM results as r0
  |]
