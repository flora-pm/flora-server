{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageIndex.Query where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL

import Flora.Database
import Flora.Model.PackageIndex.Types

getPackageIndexById
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es)
  => PackageIndexId
  -> Eff es (Maybe PackageIndex)
getPackageIndexById packageIndexId =
  labeled @ReadOnly @WithConnection $
    queryOne (_selectWhere @PackageIndex [[field| package_index_id |]]) (Only packageIndexId)

getPackageIndexByName :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => Text -> Eff es (Maybe PackageIndex)
getPackageIndexByName repository =
  labeled @ReadOnly @WithConnection $
    queryOne (_selectWhere @PackageIndex [[field| repository |]]) (Only repository)

listPackageIndexes :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => Eff es (Vector PackageIndex)
listPackageIndexes =
  labeled @ReadOnly @WithConnection $ Vector.fromList <$> query_ (_select @PackageIndex <> _orderByMany [([field| repository |], ASC)])

-- | Returns an ordered list of index dependencies, which must be
-- traversed **in order** to determine the provenance of a dependency.
getIndexDependencies :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => PackageIndexId -> Eff es (Vector Text)
getIndexDependencies packageIndexId = do
  result' <-
    labeled @ReadOnly @WithConnection $
      Vector.fromList
        <$> query q (Only packageIndexId)
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
