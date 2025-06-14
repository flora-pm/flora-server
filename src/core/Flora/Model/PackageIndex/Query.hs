{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageIndex.Query where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Flora.Model.PackageIndex.Types

getPackageIndexByName :: DB :> es => Text -> Eff es (Maybe PackageIndex)
getPackageIndexByName repository =
  let index = case repository of
        "haskell" -> "hackage"
        r -> r
   in dbtToEff $
        selectOneByField [field| repository |] (Only index)

listPackageIndexes :: DB :> es => Eff es (Vector PackageIndex)
listPackageIndexes =
  dbtToEff $
    selectOrderBy @PackageIndex $
      Vector.fromList [([field| repository |], ASC)]

-- | Returns an ordered list of index dependencies, which must be
-- traversed **in order** to determine the provenance of a dependency.
getIndexDependencies :: DB :> es => PackageIndexId -> Eff es (Vector Text)
getIndexDependencies packageIndexId = do
  result' <-
    dbtToEff $
      query q (Only packageIndexId)
  pure $ fromOnly <$> result'
  where
    q =
      [sql|
      SELECT p1.repository
      FROM index_dependencies AS i0
           INNER JOIN package_indexes AS p1 ON i0.dependency = p1.package_index_id
      WHERE i0.dependent = ?
      ORDER BY i0.priority
    |]
