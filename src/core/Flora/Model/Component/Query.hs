{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Component.Query
  ( getCanonicalComponentByReleaseId
  , getPackageComponentByReleaseId
  ) where

import Data.Text
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT
  ( QueryNature (Select)
  , query
  )
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (..))
import Effectful
import Effectful.PostgreSQL.Transact.Effect

import Flora.Model.Component.Types
import Flora.Model.Release.Types (ReleaseId)

getCanonicalComponentByReleaseId :: DB :> es => ReleaseId -> Eff es (Vector CanonicalComponent)
getCanonicalComponentByReleaseId releaseId = do
  (results :: Vector (Text, ComponentType)) <-
    dbtToEff $
      query
        Select
        ( _selectWithFields @PackageComponent
            [[field| component_name |], [field| component_type |]]
            <> _where [[field| release_id |]]
        )
        (Only releaseId)
  pure $ fmap (uncurry CanonicalComponent) results

getPackageComponentByReleaseId :: DB :> es => ReleaseId -> Eff es (Vector PackageComponent)
getPackageComponentByReleaseId releaseId = dbtToEff $ selectManyByField @PackageComponent [field| release_id |] (Only releaseId)
