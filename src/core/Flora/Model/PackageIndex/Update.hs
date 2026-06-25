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
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Effectful
import Heptapod qualified

import Data.Positive
import Flora.Database
import Flora.Model.PackageIndex.Types
  ( PackageIndex (..)
  , PackageIndexId
  , mkPackageIndex
  )

updatePackageIndexByName :: (IOE :> es, WriteDB :> es) => Text -> Maybe UTCTime -> Eff es ()
updatePackageIndexByName repositoryName newTimestamp = do
  void $
    execute
      ( _updateFieldsBy @PackageIndex
          [[field| timestamp |]]
          [field| repository |]
      )
      ( toRow (Only newTimestamp)
          ++ toRow (Only repositoryName)
      )

createPackageIndex :: (IOE :> es, WriteDB :> es) => Text -> Text -> Text -> Maybe UTCTime -> Eff es ()
createPackageIndex repositoryName url description timestamp = do
  packageIndex <- mkPackageIndex repositoryName url description timestamp
  void $ execute (_insert @PackageIndex) packageIndex

upsertPackageIndex :: (IOE :> es, WriteDB :> es) => Text -> Text -> Text -> Maybe UTCTime -> Eff es ()
upsertPackageIndex repositoryName url description timestamp = do
  packageIndex <- mkPackageIndex repositoryName url description timestamp
  void $ execute (_insert @PackageIndex <> " ON CONFLICT DO NOTHING") packageIndex

addDependency
  :: (IOE :> es, WriteDB :> es)
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
