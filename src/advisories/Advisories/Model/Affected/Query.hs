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
import Security.Advisories.Core.HsecId

import Advisories.HsecId.Orphans ()
import Advisories.Model.Advisory.Types
import Advisories.Model.Affected.Types
import Flora.Database
import Flora.Model.Package.Types

getAffectedPackageById
  :: (IOE :> es, ReadDB :> es)
  => AffectedPackageId
  -> Eff es (Maybe AffectedPackageDAO)
getAffectedPackageById affectedPackageId = queryOne (_selectWhere @AffectedPackageDAO [primaryKey @AffectedPackageDAO]) (Only affectedPackageId)

getAffectedPackagesByAdvisoryId
  :: (IOE :> es, ReadDB :> es)
  => AdvisoryId
  -> Eff es (Vector AffectedPackageDAO)
getAffectedPackagesByAdvisoryId advisoryId =
  Vector.fromList <$> query (_selectWhere @AffectedPackageDAO [[field| advisory_id |]]) (Only advisoryId)

getAffectedPackagesByHsecId
  :: (IOE :> es, ReadDB :> es)
  => HsecId
  -> Eff es (Vector AffectedPackageDAO)
getAffectedPackagesByHsecId hsecId =
  Vector.fromList
    <$> query (_joinSelectOneByField @AffectedPackageDAO @AdvisoryDAO [field| advisory_id |] [field| hsec_id |]) (Only hsecId)

getAdvisoryPreviewsByPackageId :: (IOE :> es, ReadDB :> es) => PackageId -> Eff es (Vector PackageAdvisoryPreview)
getAdvisoryPreviewsByPackageId packageId =
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

searchInAdvisories :: (IOE :> es, ReadDB :> es) => (Word, Word) -> Text -> Eff es (Vector PackageAdvisoryPreview)
searchInAdvisories (offset, limit) searchTerm =
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

countAdvisorySearchResults :: (IOE :> es, ReadDB :> es) => Text -> Eff es Word
countAdvisorySearchResults searchTerm =
  queryCount countAdvisorySearchResultsQuery (searchTerm, searchTerm)

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

getAdvisoryPreviewsByNamespace
  :: (IOE :> es, ReadDB :> es)
  => (Word, Word)
  -- ^ (offset, limit)
  -> Namespace
  -> Eff es (Vector PackageAdvisoryPreview)
getAdvisoryPreviewsByNamespace (offset, limit) namespace =
  Vector.fromList
    <$> query
      getAdvisoryPreviewsByNamespaceQuery
      (namespace, offset, limit)

getAdvisoryPreviewsByNamespaceQuery :: Query
getAdvisoryPreviewsByNamespaceQuery =
  [sql|
WITH results AS (
  SELECT DISTINCT ON (s0.hsec_id)
         s0.hsec_id
       , p3.namespace
       , p3.name
       , s0.summary
       , CASE
           WHEN EXISTS (
             SELECT 1
             FROM affected_version_ranges AS vr
             WHERE vr.affected_package_id = a1.affected_package_id
               AND vr.fixed_version IS NOT NULL
           )
             THEN TRUE
           ELSE FALSE
         END as fixed
       , s0.published
       , a1.cvss
  FROM security_advisories AS s0
       INNER JOIN affected_packages AS a1 ON s0.advisory_id = a1.advisory_id
       INNER JOIN packages AS p3 ON a1.package_id = p3.package_id
  WHERE p3.namespace = ?
  ORDER BY s0.hsec_id, p3.package_id
)

SELECT r0.hsec_id
     , r0.namespace
     , r0.name
     , r0.summary
     , r0.fixed
     , r0.published
     , r0.cvss
FROM results AS r0
ORDER BY r0.published DESC
OFFSET ?
LIMIT ?
  |]

countAdvisoriesInNamespace :: (IOE :> es, ReadDB :> es) => Namespace -> Eff es Word
countAdvisoriesInNamespace = queryCount countAdvisoriesInNamespaceQuery . Only

countAdvisoriesInNamespaceQuery :: Query
countAdvisoriesInNamespaceQuery =
  [sql|
SELECT count(DISTINCT s0.hsec_id)
FROM security_advisories AS s0
     INNER JOIN affected_packages AS a1 ON s0.advisory_id = a1.advisory_id
     INNER JOIN packages AS p3 ON a1.package_id = p3.package_id
WHERE p3.namespace = ?
  |]

getAdvisoryPreviewsByPackage
  :: (IOE :> es, ReadDB :> es)
  => (Word, Word)
  -- ^ (offset, limit)
  -> Namespace
  -> PackageName
  -> Eff es (Vector PackageAdvisoryPreview)
getAdvisoryPreviewsByPackage (offset, limit) namespace packageName =
  Vector.fromList
    <$> query
      getAdvisoryPreviewsByPackageQuery
      (namespace, packageName, offset, limit)

getAdvisoryPreviewsByPackageQuery :: Query
getAdvisoryPreviewsByPackageQuery =
  [sql|
WITH results AS (
  SELECT DISTINCT ON (s0.hsec_id)
         s0.hsec_id
       , p3.namespace
       , p3.name
       , s0.summary
       , CASE
           WHEN EXISTS (
             SELECT 1
             FROM affected_version_ranges AS vr
             WHERE vr.affected_package_id = a1.affected_package_id
               AND vr.fixed_version IS NOT NULL
           )
             THEN TRUE
           ELSE FALSE
         END as fixed
       , s0.published
       , a1.cvss
  FROM security_advisories AS s0
       INNER JOIN affected_packages AS a1 ON s0.advisory_id = a1.advisory_id
       INNER JOIN packages AS p3 ON a1.package_id = p3.package_id
  WHERE p3.namespace = ?
    AND p3.name = ?
  ORDER BY s0.hsec_id, p3.package_id
)

SELECT r0.hsec_id
     , r0.namespace
     , r0.name
     , r0.summary
     , r0.fixed
     , r0.published
     , r0.cvss
FROM results AS r0
ORDER BY r0.published DESC
OFFSET ?
LIMIT ?
  |]

countAdvisoriesForPackage :: (IOE :> es, ReadDB :> es) => Namespace -> PackageName -> Eff es Word
countAdvisoriesForPackage namespace packageName =
  queryCount countAdvisoriesForPackageQuery (namespace, packageName)

countAdvisoriesForPackageQuery :: Query
countAdvisoriesForPackageQuery =
  countAdvisoriesInNamespaceQuery
    <> [sql|
     AND p3.name = ?
  |]

getAffectedPackageInfosByHsecId :: (IOE :> es, ReadDB :> es) => HsecId -> Eff es (Vector AffectedPackageInfo)
getAffectedPackageInfosByHsecId hsecId =
  Vector.fromList
    <$> query
      [sql|
SELECT p3.namespace
     , p3.name
     , a1.cvss
     , CASE
         WHEN a2.fixed_version IS NULL
           THEN FALSE
         ELSE TRUE
       END as fixed
FROM affected_packages AS a1
     INNER JOIN security_advisories AS s0 ON a1.advisory_id = s0.advisory_id
     INNER JOIN packages AS p3 ON a1.package_id = p3.package_id
     LEFT JOIN affected_version_ranges AS a2 ON a1.affected_package_id = a2.affected_package_id
WHERE s0.hsec_id = ?
GROUP BY p3.namespace, p3.name, a1.cvss, fixed
  |]
      (Only hsecId)
