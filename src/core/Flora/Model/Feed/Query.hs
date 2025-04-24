{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Feed.Query where

import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple.SqlQQ
import Effectful
import Effectful.PostgreSQL.Transact.Effect

import Flora.Model.Feed.Types
import Flora.Model.Package.Types

getEntriesByPackage
  :: DB :> es
  => Namespace
  -> PackageName
  -> Word
  -- ^ Offset
  -> Word
  -- ^ Limit
  -> Eff es (Vector FeedEntry)
getEntriesByPackage namespace package offset limit = do
  dbtToEff $ query Select querySpec (namespace, package, offset, limit)
  where
    querySpec =
      [sql|
        SELECT f0.entry_id
             , f0.title
             , f0.link
             , f0.content
             , f0.package_id
             , f0.created_at
             , f0.updated_at
        FROM feed_entries AS f0
             INNER JOIN latest_versions AS l1 ON l1.namespace = ?
                                             AND l1.name = ?
        WHERE l1.package_id = f0.package_id
        OFFSET ?
        LIMIT ?
    |]
