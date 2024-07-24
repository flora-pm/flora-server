{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Release.Query
  ( getReleases
  , getReleaseTarballRootHash
  , getReleaseTarballArchive
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

import Control.Monad (join)
import Data.ByteString (fromStrict)
import Data.ByteString.Lazy (LazyByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query, queryOne, queryOne_, query_)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (In (..), Only (..), Query)
import Database.PostgreSQL.Simple.SqlQQ
import Distribution.Orphans.Version ()
import Distribution.Version (Version)
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Flora.Model.BlobStore.API (BlobStoreAPI, get)
import Flora.Model.BlobStore.Types
import Flora.Model.Component.Types
import Flora.Model.Package.Types
import Flora.Model.Release.Types

packageReleasesQuery :: Query
packageReleasesQuery =
  _selectWhere @Release [[field| package_id |]]
    <> " ORDER BY releases.version DESC "

getReleases :: DB :> es => PackageId -> Eff es (Vector Release)
getReleases pid =
  dbtToEff $ do
    query Select (packageReleasesQuery <> " LIMIT 6") (Only pid)

getLatestReleaseTime :: DB :> es => Maybe Text -> Eff es (Maybe UTCTime)
getLatestReleaseTime repo =
  dbtToEff $ fmap fromOnly <$> maybe (queryOne_ Select q') (queryOne Select q . Only) repo
  where
    q = [sql| select max(r0.uploaded_at) from releases as r0 where r0.repository = ? |]
    q' = [sql| select max(uploaded_at) from releases |]

getReleaseTarballRootHash :: DB :> es => ReleaseId -> Eff es (Maybe Sha256Sum)
getReleaseTarballRootHash releaseId = dbtToEff $ do
  mRelease <- selectOneByField @Release [field| release_id |] (Only releaseId)
  case mRelease of
    Just release -> pure $ tarballRootHash release
    Nothing -> error $ "Internal error: searched for releaseId that doesn't exist: " <> show releaseId

getReleaseTarballArchive :: (BlobStoreAPI :> es, DB :> es) => ReleaseId -> Eff es (Maybe LazyByteString)
getReleaseTarballArchive releaseId = do
  mRelease <- dbtToEff $ selectOneByField @Release [field| release_id |] (Only releaseId)
  case mRelease of
    Nothing -> error $ "Internal error: searched for releaseId that doesn't exist: " <> show releaseId
    Just release -> do
      fmap fromStrict . join <$> traverse get release.tarballArchiveHash

getAllReleases :: DB :> es => PackageId -> Eff es (Vector Release)
getAllReleases pid =
  dbtToEff $ do
    query Select packageReleasesQuery (Only pid)

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
        where r.tarball_root_hash is null
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
