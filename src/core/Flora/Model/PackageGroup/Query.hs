{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageGroup.Query
  ( getPackagesByPackageGroupId
  , getPackageGroupByPackageGroupName
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (selectOneByField)
import Database.PostgreSQL.Entity.DBT
  ( QueryNature (Select)
  , query
  )
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful (Eff, type (:>))
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Flora.Model.Package.Types (Package)
import Flora.Model.PackageGroup.Types (PackageGroup, PackageGroupId)

getPackagesByPackageGroupId :: DB :> es => PackageGroupId -> Eff es (Vector Package)
getPackagesByPackageGroupId packageGroupId = dbtToEff $ query Select q (Only packageGroupId)
  where
    q =
      [sql|
        SELECT p.*
        FROM packages AS p
        JOIN package_group_packages AS pgp
        ON p.package_id = pgp.package_id
        WHERE pgp.package_group_id = ?
      |]

getPackageGroupByPackageGroupName :: DB :> es => Text -> Eff es (Maybe PackageGroup)
getPackageGroupByPackageGroupName groupName = dbtToEff $ selectOneByField [field| group_name |] (Only groupName)
