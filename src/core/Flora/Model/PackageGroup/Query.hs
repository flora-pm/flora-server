{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageGroup.Query
  ( getPackagesByPackageGroupId
  , getPackageGroupByPackageGroupName
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (joinSelectOneByField, selectOneByField)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))
import Effectful (Eff, type (:>))
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Flora.Model.Package.Types (Package)
import Flora.Model.PackageGroup.Types (PackageGroup (..), PackageGroupId (..))
import Flora.Model.PackageGroupPackage.Types (PackageGroupPackage (..))

getPackagesByPackageGroupId :: DB :> es => PackageGroupId -> Eff es (Vector Package)
getPackagesByPackageGroupId packageGroupId =
  dbtToEff $
    joinSelectOneByField
      @Package
      @PackageGroupPackage
      [field| package_id |]
      [field| package_group_id |]
      packageGroupId

getPackageGroupByPackageGroupName :: DB :> es => Text -> Eff es (Maybe PackageGroup)
getPackageGroupByPackageGroupName groupName = dbtToEff $ selectOneByField [field| group_name |] (Only groupName)
