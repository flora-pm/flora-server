{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Release.Query
  ( getReleases
  , getRelease
  , getReleaseTarball
  , getReleaseByVersion
  , getHackagePackageReleasesWithoutReadme
  , getHackagePackageReleasesWithoutChangelog
  , getHackagePackageReleasesWithoutUploadTimestamp
  , getHackagePackageReleasesWithoutTarball
  , getAllReleases
  , getLatestReleaseTime
  , getNumberOfReleases
  , getReleaseComponents
  , getHackagePackagesWithoutReleaseDeprecationInformation
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

import Distribution.Orphans.Version ()
import Flora.Model.BlobStore.Types
import Flora.Model.Component.Types
import Flora.Model.Package.Types
import Flora.Model.Release.Types

packageReleasesQuery :: Query
packageReleasesQuery = _selectWhere @Release [[field| package_id |]]

getReleases :: DB :> es => PackageId -> Eff es (Vector Release)
getReleases pid =
  dbtToEff $ do
    results <- query Select packageReleasesQuery (Only pid)
    if Vector.null results
      then pure results
      else pure $ Vector.take 6 $ Vector.reverse $ Vector.modify MVector.sort results

getLatestReleaseTime :: DB :> es => Maybe Text -> Eff es (Maybe UTCTime)
getLatestReleaseTime repo =
  dbtToEff $ fmap fromOnly <$> maybe (queryOne_ Select q') (queryOne Select q . Only) repo
  where
    q = [sql| select max(r0.uploaded_at) from releases as r0 where r0.repository = ? |]
    q' = [sql| select max(uploaded_at) from releases |]

getReleaseTarball :: DB :> es => ReleaseId -> Eff es (Maybe Sha256Sum)
getReleaseTarball releaseId = dbtToEff $ do
  mRelease <- selectOneByField @Release [field| release_id |] (Only releaseId)
  case mRelease of
    Just release -> pure $ tarball release
    Nothing -> error $ "Internal error: searched for releaseId that doesn't exist: " <> show releaseId

getAllReleases :: DB :> es => PackageId -> Eff es (Vector Release)
getAllReleases pid =
  dbtToEff $ do
    results <- query Select packageReleasesQuery (Only pid)
    if Vector.null results
      then pure results
      else pure $ Vector.reverse $ Vector.modify MVector.sort results

getVersionFromManyReleaseIds
  :: DB :> es
  => Vector ReleaseId
  -> Eff es (Vector (ReleaseId, Version))
getVersionFromManyReleaseIds releaseIds = do
  dbtToEff $ query Select q (Only (In (Vector.toList releaseIds)))
  where
    q =
      [sql|
        select r0.release_id, r0.version
        from releases as r0
        where r0.release_id in ?
      |]

getHackagePackageReleasesWithoutReadme
  :: DB :> es
  => Eff es (Vector (ReleaseId, Version, PackageName))
getHackagePackageReleasesWithoutReadme =
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
          and p.namespace = 'hackage'
           or p.namespace = 'haskell'
      |]

getHackagePackageReleasesWithoutUploadTimestamp
  :: DB :> es
  => Eff es (Vector (ReleaseId, Version, PackageName))
getHackagePackageReleasesWithoutUploadTimestamp =
  dbtToEff $
    query Select querySpec ()
  where
    querySpec :: Query
    querySpec =
      [sql|
        select r."release_id", r."version", p."name"
        from releases as r
        join packages as p
        on p."package_id" = r."package_id"
        where r."uploaded_at" is null
          and p."namespace" = 'hackage'
           or p."namespace" = 'haskell'
      |]

getHackagePackageReleasesWithoutChangelog
  :: DB :> es
  => Eff es (Vector (ReleaseId, Version, PackageName))
getHackagePackageReleasesWithoutChangelog =
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
          and p.namespace = 'hackage'
           or p.namespace = 'haskell'
      |]

getHackagePackageReleasesWithoutTarball
  :: DB :> es
  => Eff es (Vector (ReleaseId, Version, PackageName))
getHackagePackageReleasesWithoutTarball =
  dbtToEff $! query Select querySpec ()
  where
    querySpec =
      [sql|
        select r.release_id, r.version, p.name
        from releases as r
        join packages as p
        on p.package_id = r.package_id
        where r.tarball is null
      |]

getHackagePackagesWithoutReleaseDeprecationInformation
  :: DB :> es
  => Eff es (Vector (PackageName, Vector ReleaseId))
getHackagePackagesWithoutReleaseDeprecationInformation =
  dbtToEff $ query_ Select q
  where
    q =
      [sql|
        select p1.name, array_agg(r0.release_id)
        from releases as r0
        join packages as p1 on r0.package_id = p1.package_id
        where r0.deprecated is null
          and p1.namespace = 'hackage'
           or p1.namespace = 'haskell'
        group by p1.name;
        |]

getReleaseByVersion
  :: DB :> es
  => PackageId
  -> Version
  -> Eff es (Maybe Release)
getReleaseByVersion packageId version =
  dbtToEff $
    queryOne Select (_selectWhere @Release [[field| package_id |], [field| version |]]) (packageId, version)

getRelease
  :: DB :> es
  => Namespace
  -> PackageName
  -> Version
  -> Eff es (Maybe Release)
getRelease namespace packageName version =
  dbtToEff $
    queryOne
      Select
      (_selectWhere @Release [[field| namespace |], [field| package_name |], [field| version |]])
      (namespace, packageName, version)

getNumberOfReleases :: DB :> es => PackageId -> Eff es Word
getNumberOfReleases pid =
  dbtToEff $ do
    (result :: Maybe (Only Int)) <- queryOne Select numberOfReleasesQuery (Only pid)
    case result of
      Just (Only n) -> pure $ fromIntegral n
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
  dbtToEff $ query Select (_selectWhere @PackageComponent [[field| release_id |]]) (Only releaseId)
