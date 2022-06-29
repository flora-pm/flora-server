{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Release.Query
  ( getReleases
  , getReleaseByVersion
  , getPackageReleases
  , getAllReleases
  , getNumberOfReleases
  )
where

import Control.Monad.IO.Class
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Algorithms.Intro as MVector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query, queryOne)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..), Query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import Distribution.Make (Version)
import Flora.Model.Package.Types
import Flora.Model.Release.Types

packageReleasesQuery :: Query
packageReleasesQuery = _selectWhere @Release [[field| package_id |]]

getReleases :: MonadIO m => PackageId -> DBT m (Vector Release)
getReleases pid = do
  results <- query Select packageReleasesQuery (Only pid)
  if Vector.null results
    then pure results
    else pure $ Vector.take 6 $ Vector.reverse $ Vector.modify MVector.sort results

getAllReleases :: MonadIO m => PackageId -> DBT m (Vector Release)
getAllReleases pid = do
  results <- query Select packageReleasesQuery (Only pid)
  if Vector.null results
    then pure results
    else pure $ Vector.reverse $ Vector.modify MVector.sort results

getPackageReleases :: MonadIO m => DBT m (Vector (ReleaseId, Version, PackageName))
getPackageReleases =
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

getReleaseByVersion :: MonadIO m => PackageId -> Version -> DBT m (Maybe Release)
getReleaseByVersion packageId version = queryOne Select (_selectWhere @Release [[field| package_id |], [field| version |]]) (packageId, version)

getNumberOfReleases :: MonadIO m => PackageId -> DBT m Word
getNumberOfReleases pid = do
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
