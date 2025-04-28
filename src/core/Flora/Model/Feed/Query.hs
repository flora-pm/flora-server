{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Feed.Query where

import Data.List (List)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple (In (..))
import Database.PostgreSQL.Simple.SqlQQ
import Effectful
import Effectful.PostgreSQL.Transact.Effect

import Flora.Model.Feed.Types
import Flora.Model.Package.Types

getEntriesByPackage
  :: DB :> es
  => List (Namespace, PackageName)
  -> Word
  -- ^ Offset
  -> Word
  -- ^ Limit
  -> Eff es (Vector FeedEntry)
getEntriesByPackage packages offset limit = do
  dbtToEff $ query Select querySpec (In packages, offset, limit)
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
        FROM package_feeds AS f0
             INNER JOIN latest_versions AS l1 ON (l1.namespace, l1.name) IN ?
        WHERE l1.package_id = f0.package_id
        ORDER BY f0.updated_at DESC
        OFFSET ?
        LIMIT ?
    |]
