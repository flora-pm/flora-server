{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageIndex.Update
  ( updatePackageIndexByName
  , createPackageIndex
  ) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Entity (insert, updateFieldsBy)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (..))
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Flora.Model.PackageIndex.Types
  ( PackageIndex (..)
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

createPackageIndex :: (IOE :> es, DB :> es) => Text -> Text -> Text -> Maybe UTCTime -> Eff es ()
createPackageIndex repositoryName url description timestamp = do
  packageIndex <- mkPackageIndex repositoryName url description timestamp
  void $ dbtToEff $ insert @PackageIndex packageIndex
