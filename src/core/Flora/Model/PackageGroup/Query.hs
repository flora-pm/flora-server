{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageGroup.Query
  ( getPackagesByPackageGroupId
  , getPackageGroupByPackageGroupName
  ) where

import Data.Text (Text)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (Only))
import Effectful
import Effectful.PostgreSQL.Transact.Effect
import Flora.Model.PackageGroup.Types (PackageGroup, PackageGroupId)

getPackagesByPackageGroupId :: DB :> es => PackageGroupId -> Eff es (Maybe PackageGroup)
getPackagesByPackageGroupId packageGroupId = dbtToEff $ selectById (Only packageGroupId)

getPackageGroupByPackageGroupName :: DB :> es => Text -> Eff es (Maybe PackageGroup)
getPackageGroupByPackageGroupName packageGroupName = dbtToEff $ selectOneByField [field| packageGroupName |] (Only packageGroupName)
