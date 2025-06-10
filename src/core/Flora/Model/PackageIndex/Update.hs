{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageIndex.Update
  ( updatePackageIndexByName
  , createPackageIndex
  , upsertPackageIndex
  , addDependency
  ) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Entity (insert, updateFieldsBy, _insert)
import Database.PostgreSQL.Entity.DBT (execute)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Heptapod qualified

import Data.Positive
import Flora.Model.PackageIndex.Types
  ( PackageIndex (..)
  , PackageIndexId
  , mkPackageIndex
  )

updatePackageIndexByName :: DB :> es => Text -> Maybe UTCTime -> Eff es ()
updatePackageIndexByName repositoryName newTimestamp = do
  void $
    dbtToEff $
      updateFieldsBy @PackageIndex
        [[field| timestamp |]]
        ([field| repository |], repositoryName)
        (Only newTimestamp)

createPackageIndex :: (DB :> es, IOE :> es) => Text -> Text -> Text -> Maybe UTCTime -> Eff es ()
createPackageIndex repositoryName url description timestamp = do
  packageIndex <- mkPackageIndex repositoryName url description timestamp
  void $ dbtToEff $ insert @PackageIndex packageIndex

upsertPackageIndex :: (DB :> es, IOE :> es) => Text -> Text -> Text -> Maybe UTCTime -> Eff es ()
upsertPackageIndex repositoryName url description timestamp = do
  packageIndex <- mkPackageIndex repositoryName url description timestamp
  dbtToEff $ void $ execute (_insert @PackageIndex <> " ON CONFLICT DO NOTHING") packageIndex

addDependency
  :: (DB :> es, IOE :> es)
  => PackageIndexId
  -- ^ Index
  -> PackageIndexId
  -- ^ Dependency
  -> Positive Word
  -- ^ Priority
  -> Eff es ()
addDependency indexId dependencyId priority = do
  indexDependencyId <- liftIO Heptapod.generate
  void $
    dbtToEff $
      execute q (indexDependencyId, indexId, dependencyId, priority)
  where
    q =
      [sql|
    INSERT INTO index_dependencies (index_dependency_id
                                  , dependent
                                  , dependency
                                  , priority)
    VALUES (?, ?, ?, ?)
    |]
