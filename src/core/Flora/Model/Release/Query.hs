{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Release.Query
  ( getReleases
  , getReleaseByVersion
  , getPackageReleases
  , getPackageReleasesWithoutReadme
  , getPackageReleasesWithoutChangelog
  , getPackageReleasesWithoutUploadTimestamp
  , getAllReleases
  , getLatestReleaseTime
  , getNumberOfReleases
  , getReleaseComponents
  , getPackagesWithoutReleaseDeprecationInformation
  , getVersionFromManyReleaseIds
  )
where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Algorithms.Intro as MVector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query, queryOne, queryOne_, query_)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (In (..), Only (..), Query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Distribution.Version (Version)
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Flora.Model.Package.Component
import Flora.Model.Package.Types
import Flora.Model.Release.Types

packageReleasesQuery :: Query
packageReleasesQuery = _selectWhere @Release [[field| package_id |]]

getReleases :: DB :> es => PackageId -> Eff es (Vector Release)
getReleases pid =
  dbtToEff $! do
    results <- query Select packageReleasesQuery (Only pid)
    if Vector.null results
      then pure results
      else pure $! Vector.take 6 $! Vector.reverse $! Vector.modify MVector.sort results

getLatestReleaseTime :: DB :> es => Maybe Text -> Eff es (Maybe UTCTime)
getLatestReleaseTime repo =
  dbtToEff $! fmap fromOnly <$> maybe (queryOne_ Select q') (queryOne Select q . Only) repo
  where
    q = [sql| select max(r0.uploaded_at) from releases as r0 where r0.repository = ? |]
    q' = [sql| select max(uploaded_at) from releases |]

getAllReleases :: DB :> es => PackageId -> Eff es (Vector Release)
getAllReleases pid =
  dbtToEff $! do
    results <- query Select packageReleasesQuery (Only pid)
    if Vector.null results
      then pure results
      else pure $! Vector.reverse $! Vector.modify MVector.sort results

getVersionFromManyReleaseIds
  :: DB :> es
  => Vector ReleaseId
  -> Eff es (Vector (ReleaseId, Version))
getVersionFromManyReleaseIds releaseIds = do
  dbtToEff $! query Select q (Only (In (Vector.toList releaseIds)))
  where
    q =
      [sql|
        select r0.release_id, r0.version
        from releases as r0
        where r0.release_id in ?
      |]

getPackageReleases :: DB :> es => Eff es (Vector (ReleaseId, Version, PackageName))
getPackageReleases =
  dbtToEff $
    query Select querySpec ()
  where
    querySpec :: Query
    querySpec =
      [sql|
        select r.release_id, r.version, p."name"
        from releases as r
        join packages as p
        on p.package_id = r.package_id
      |]

getPackageReleasesWithoutReadme
  :: DB :> es
  => Eff es (Vector (ReleaseId, Version, PackageName))
getPackageReleasesWithoutReadme =
  dbtToEff $
    query Select querySpec ()
  where
    querySpec :: Query
    querySpec =
      [sql|
        select r.release_id, r.version, p."name"
        from releases as r
        join packages as p
        on p.package_id = r.package_id
        where r.readme_status = 'not-imported'
      |]

getPackageReleasesWithoutUploadTimestamp
  :: DB :> es
  => Eff es (Vector (ReleaseId, Version, PackageName))
getPackageReleasesWithoutUploadTimestamp =
  dbtToEff $
    query Select querySpec ()
  where
    querySpec :: Query
    querySpec =
      [sql|
        select r.release_id, r.version, p."name"
        from releases as r
        join packages as p
        on p.package_id = r.package_id
        where r.uploaded_at is null
      |]

getPackageReleasesWithoutChangelog
  :: DB :> es
  => Eff es (Vector (ReleaseId, Version, PackageName))
getPackageReleasesWithoutChangelog =
  dbtToEff $
    query Select querySpec ()
  where
    querySpec :: Query
    querySpec =
      [sql|
        select r.release_id, r.version, p."name"
        from releases as r
        join packages as p
        on p.package_id = r.package_id
        where r.changelog_status = 'not-imported'
      |]

getPackagesWithoutReleaseDeprecationInformation
  :: DB :> es
  => Eff es (Vector (PackageName, Vector ReleaseId))
getPackagesWithoutReleaseDeprecationInformation =
  dbtToEff $! query_ Select q
  where
    q =
      [sql|
        select p1.name, array_agg(r0.release_id)
        from releases as r0
        join packages as p1 on r0.package_id = p1.package_id
        where r0.deprecated is null
        group by p1.name;
        |]

getReleaseByVersion
  :: DB :> es
  => PackageId
  -> Version
  -> Eff es (Maybe Release)
getReleaseByVersion packageId version =
  dbtToEff $!
    queryOne Select (_selectWhere @Release [[field| package_id |], [field| version |]]) (packageId, version)

getNumberOfReleases :: DB :> es => PackageId -> Eff es Word
getNumberOfReleases pid =
  dbtToEff $! do
    (result :: Maybe (Only Int)) <- queryOne Select numberOfReleasesQuery (Only pid)
    case result of
      Just (Only n) -> pure $! fromIntegral n
      Nothing -> pure 0

numberOfReleasesQuery :: Query
numberOfReleasesQuery =
  [sql|
  SELECT DISTINCT COUNT(rel."package_id")
  FROM releases AS rel
  WHERE rel."package_id" = ?
  |]

getReleaseComponents :: DB :> es => ReleaseId -> Eff es (Vector PackageComponent)
getReleaseComponents releaseId =
  dbtToEff $! query Select (_selectWhere @PackageComponent [[field| release_id |]]) (Only releaseId)
