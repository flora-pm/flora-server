{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Advisories.Model.Affected.Query where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..), Query)
import Database.PostgreSQL.Simple.SqlQQ
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL
import Security.Advisories.Core.HsecId

import Advisories.HsecId.Orphans ()
import Advisories.Model.Advisory.Types
import Advisories.Model.Affected.Types
import Flora.Database
import Flora.Model.Package.Types

getAffectedPackageById
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => AffectedPackageId
  -> Eff es (Maybe AffectedPackageDAO)
getAffectedPackageById affectedPackageId = labeled @ReadOnly @WithConnection $ queryOne (_selectWhere @AffectedPackageDAO [primaryKey @AffectedPackageDAO]) (Only affectedPackageId)

getAffectedPackagesByAdvisoryId
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => AdvisoryId
  -> Eff es (Vector AffectedPackageDAO)
getAffectedPackagesByAdvisoryId advisoryId =
  labeled @ReadOnly @WithConnection $ Vector.fromList <$> query (_selectWhere @AffectedPackageDAO [[field| advisory_id |]]) (Only advisoryId)

getAffectedPackagesByHsecId
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => HsecId
  -> Eff es (Vector AffectedPackageDAO)
getAffectedPackagesByHsecId hsecId =
  labeled @ReadOnly @WithConnection $
    Vector.fromList
      <$> query (_joinSelectOneByField @AffectedPackageDAO @AdvisoryDAO [field| advisory_id |] [field| hsec_id |]) (Only hsecId)

getAdvisoryPreviewsByPackageId :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => PackageId -> Eff es (Vector PackageAdvisoryPreview)
getAdvisoryPreviewsByPackageId packageId =
  labeled @ReadOnly @WithConnection $
    Vector.fromList
      <$> query
        [sql|
SELECT s0.hsec_id
     , p3.namespace
     , p3.name
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
GROUP BY s0.hsec_id, p3.namespace, p3.name, s0.summary, fixed, s0.published, a1.cvss
  |]
        (Only packageId)

searchInAdvisories :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => (Word, Word) -> Text -> Eff es (Vector PackageAdvisoryPreview)
searchInAdvisories (offset, limit) searchTerm =
  labeled @ReadOnly @WithConnection $
    Vector.fromList
      <$> query
        searchAdvisoriesQuery
        (searchTerm, searchTerm, offset, limit)

searchAdvisoriesQuery :: Query
searchAdvisoriesQuery =
  [sql|
WITH results AS (
  SELECT s0.hsec_id
       , p3.namespace
       , p3.name
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
  GROUP BY s0.hsec_id, p3.namespace, p3.name, s0.summary, fixed, s0.published, a1.cvss, rating
  ORDER BY rating desc, s0.summary asc
  OFFSET ?
  LIMIT ?
)

SELECT r0.hsec_id
     , r0.namespace
     , r0.name
     , r0.summary
     , r0.fixed
     , r0.published
     , r0.cvss
FROM results as r0
  |]

countAdvisorySearchResults :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => Text -> Eff es Word
countAdvisorySearchResults searchTerm =
  labeled @ReadOnly @WithConnection $ do
    (result :: Maybe (Only Int)) <-
      queryOne
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
       , p3.namespace
       , p3.name
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
