{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Component.Query (getComponentsByReleaseId) where

import Data.Text
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (query)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (..))
import Effectful
import Effectful.PostgreSQL.Transact.Effect

import Flora.Model.Component.Types
import Flora.Model.Release.Types (ReleaseId)

getComponentsByReleaseId :: DB :> es => ReleaseId -> Eff es (Vector CanonicalComponent)
getComponentsByReleaseId releaseId = do
  (results :: Vector (Text, ComponentType)) <-
    dbtToEff $
      query
        ( _selectWithFields @PackageComponent
            [[field| component_name |], [field| component_type |]]
            <> _where [[field| release_id |]]
        )
        (Only releaseId)
  pure $ fmap (uncurry CanonicalComponent) results
