{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Release.Query
  ( getReleases
  , getReleaseTarballRootHash
  , getReleaseTarballArchive
  , getReleaseById
  , getReleaseByVersion
  , getHackagePackageReleasesWithoutReadme
  , getHackagePackageReleasesWithoutChangelog
  , getHackagePackageReleasesWithoutUploadInformation
  , getHackagePackageReleasesWithoutTarball
  , getAllReleases
  , getLatestReleaseTime
  , getLatestPackageRelease
  , getNumberOfReleases
  , getReleaseComponents
  , getHackagePackagesWithoutReleaseDeprecationInformation
  , getVersionFromManyReleaseIds
  , getReleasePackageIndex
  , getLatestReleases
  , getLatestPackageReleaseVersion
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
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types (In (..), Only (..), Query)
import Distribution.Version (Version)
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL

import Distribution.Orphans.Version ()
import Flora.Database
import Flora.Model.BlobStore.API (BlobStoreAPI, get)
import Flora.Model.BlobStore.Types
import Flora.Model.Component.Types
import Flora.Model.Package.Types
import Flora.Model.PackageIndex.Types
import Flora.Model.Release.Types
import Flora.Monad

getLatestPackageReleaseQuery :: Query
getLatestPackageReleaseQuery =
  _selectWhere @Release [[field| package_id |]]
    <> " ORDER BY releases.version DESC LIMIT 1"

packageReleasesQuery :: Query
packageReleasesQuery =
  _selectWhere @Release [[field| package_id |]]
    <> " ORDER BY releases.version DESC "

getReleases :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => PackageId -> FloraM es (Vector Release)
getReleases pid =
  labeled @ReadOnly @WithConnection $
    Vector.fromList
      <$> query (packageReleasesQuery <> " LIMIT 6") (Only pid)

getLatestPackageRelease :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => PackageId -> FloraM es (Maybe Release)
getLatestPackageRelease pid =
  labeled @ReadOnly @WithConnection $ do
    queryOne getLatestPackageReleaseQuery (Only pid)

getLatestReleaseTime :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => Maybe Text -> FloraM es (Maybe UTCTime)
getLatestReleaseTime repo =
  labeled @ReadOnly @WithConnection $ fmap fromOnly <$> maybe (queryOne_ q') (queryOne q . Only) repo
  where
    q = [sql| select max(r0.uploaded_at) from releases as r0 where r0.repository = ? |]
    q' = [sql| select max(uploaded_at) from releases |]

getReleaseTarballRootHash :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => ReleaseId -> FloraM es (Maybe Sha256Sum)
getReleaseTarballRootHash releaseId = labeled @ReadOnly @WithConnection $ do
  mRelease :: Maybe Release <- queryOne (_selectWhere @Release [[field| release_id |]]) (Only releaseId)
  case mRelease of
    Just release -> pure release.tarballRootHash
    Nothing -> error $ "Internal error: searched for releaseId that doesn't exist: " <> show releaseId

getReleaseTarballArchive :: (BlobStoreAPI :> es, IOE :> es, Labeled ReadOnly WithConnection :> es) => ReleaseId -> FloraM es (Maybe LazyByteString)
getReleaseTarballArchive releaseId = labeled @ReadOnly @WithConnection $ do
  mRelease :: Maybe Release <- queryOne (_selectWhere @Release [[field| release_id |]]) (Only releaseId)
  case mRelease of
    Nothing -> error $ "Internal error: searched for releaseId that doesn't exist: " <> show releaseId
    Just release -> do
      fmap fromStrict . join <$> traverse get release.tarballArchiveHash

getAllReleases :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => PackageId -> FloraM es (Vector Release)
getAllReleases pid =
  labeled @ReadOnly @WithConnection $
    Vector.fromList
      <$> query packageReleasesQuery (Only pid)

getVersionFromManyReleaseIds
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => Vector ReleaseId
  -> FloraM es (Vector (ReleaseId, Version))
getVersionFromManyReleaseIds releaseIds = do
  labeled @ReadOnly @WithConnection $
    Vector.fromList
      <$> query q (Only (In (Vector.toList releaseIds)))
  where
    q =
      [sql|
        select r0.release_id, r0.version
        from releases as r0
        where r0.release_id in ?
      |]

getHackagePackageReleasesWithoutReadme
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => FloraM es (Vector (ReleaseId, Version, PackageName))
getHackagePackageReleasesWithoutReadme =
  labeled @ReadOnly @WithConnection $
    Vector.fromList
      <$> query querySpec ()
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
          and p.deprecation_info is null
      |]

getHackagePackageReleasesWithoutUploadInformation
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => FloraM es (Vector (ReleaseId, Version, PackageName))
getHackagePackageReleasesWithoutUploadInformation =
  labeled @ReadOnly @WithConnection $
    Vector.fromList
      <$> query querySpec ()
  where
    querySpec :: Query
    querySpec =
      [sql|
        select r."release_id", r."version", p."name"
        from releases as r
        join packages as p
        on p."package_id" = r."package_id"
        where (r."uploaded_at" is null or r."uploader_id" is null)
          and p."namespace" = 'hackage'
          and p.deprecation_info is null
      |]

getHackagePackageReleasesWithoutChangelog
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => FloraM es (Vector (ReleaseId, Version, PackageName))
getHackagePackageReleasesWithoutChangelog =
  labeled @ReadOnly @WithConnection $
    Vector.fromList
      <$> query querySpec ()
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
          and p.deprecation_info is null
      |]

getHackagePackageReleasesWithoutTarball
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => FloraM es (Vector (ReleaseId, Version, PackageName))
getHackagePackageReleasesWithoutTarball =
  labeled @ReadOnly @WithConnection $
    Vector.fromList
      <$> query querySpec ()
  where
    querySpec =
      [sql|
        select r.release_id, r.version, p.name
        from releases as r
        join packages as p
        on p.package_id = r.package_id
        where r.tarball_root_hash is null
          and p.deprecation_info is null
      |]

getHackagePackagesWithoutReleaseDeprecationInformation
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => FloraM es (Vector (PackageName, Vector ReleaseId))
getHackagePackagesWithoutReleaseDeprecationInformation =
  labeled @ReadOnly @WithConnection $ Vector.fromList <$> query_ q
  where
    q =
      [sql|
        select p1.name, array_agg(r0.release_id)
        from releases as r0
        join packages as p1 on r0.package_id = p1.package_id
        where r0.deprecated is null
          and p1.namespace = 'hackage'
          and p1.deprecation_info is null
        group by p1.name;
        |]

getReleaseById
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => ReleaseId
  -> FloraM es (Maybe Release)
getReleaseById releaseId =
  labeled @ReadOnly @WithConnection $ queryOne (_selectWhere @Release [primaryKey @Release]) (Only releaseId)

getReleaseByVersion
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => PackageId
  -> Version
  -> FloraM es (Maybe Release)
getReleaseByVersion packageId version =
  labeled @ReadOnly @WithConnection $
    queryOne
      ( _selectWhere
          @Release
          [[field| package_id |], [field| version |]]
      )
      (packageId, version)

getNumberOfReleases :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => PackageId -> FloraM es Word
getNumberOfReleases pid =
  labeled @ReadOnly @WithConnection $ do
    (result :: Maybe (Only Int)) <- queryOne numberOfReleasesQuery (Only pid)
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

getReleaseComponents :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => ReleaseId -> FloraM es (Vector PackageComponent)
getReleaseComponents releaseId =
  labeled @ReadOnly @WithConnection $
    Vector.fromList
      <$> query (_selectWhere @PackageComponent [[field| release_id |]]) (Only releaseId)

getReleasePackageIndex :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => ReleaseId -> FloraM es (Maybe PackageIndexId)
getReleasePackageIndex releaseId = labeled @ReadOnly @WithConnection $ do
  result :: Maybe (Only PackageIndexId) <- queryOne q (Only releaseId)
  pure $ fromOnly <$> result
  where
    q =
      [sql|
        select p1.package_index_id
        from releases as r0
        join package_indexes as p1 on r0.repository = p1.repository
        where r0.release_id = ?
      |]

getLatestReleases
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => FloraM es (Vector (Namespace, PackageName, Text, Version, Maybe UTCTime))
getLatestReleases =
  labeled @ReadOnly @WithConnection $
    Vector.fromList
      <$> query sqlQuery ()
  where
    sqlQuery =
      [sql|
      SELECT l0.namespace, l0.name, l0.synopsis, l0.version, l0.uploaded_at
      FROM latest_versions as l0
      ORDER BY l0.uploaded_at DESC
      LIMIT 6
      |]

getLatestPackageReleaseVersion
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => PackageId
  -> FloraM es (Maybe Version)
getLatestPackageReleaseVersion packageId = do
  result :: (Maybe (Only Version)) <- labeled @ReadOnly @WithConnection $ queryOne sqlQuery (Only packageId)
  pure $ fromOnly <$> result
  where
    sqlQuery =
      [sql|
      SELECT l0.version
      FROM latest_versions as l0
      WHERE l0.package_id = ?
      LIMIT 1
      |]
