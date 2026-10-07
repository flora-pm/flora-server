{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Component.Query (getComponentsByReleaseId) where

import Data.Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (..))
import Effectful

import Flora.Database
import Flora.Model.Component.Types
import Flora.Model.Release.Types (ReleaseId)

getComponentsByReleaseId :: (IOE :> es, ReadDB :> es) => ReleaseId -> Eff es (Vector CanonicalComponent)
getComponentsByReleaseId releaseId = do
  (results :: Vector (Text, ComponentType)) <-
    Vector.fromList
      <$> query
        ( _selectWithFields @PackageComponent
            [[field| component_name |], [field| component_type |]]
            <> _where [[field| release_id |]]
        )
        (Only releaseId)
  pure $ fmap (uncurry CanonicalComponent) results
