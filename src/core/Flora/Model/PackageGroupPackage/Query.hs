module Flora.Model.PackageGroupPackage.Query
  ( getPackageGroupPackage
  ) where

import Database.PostgreSQL.Entity (selectById)
import Database.PostgreSQL.Simple (Only (..))
import Effectful (Eff, type (:>))
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Flora.Model.PackageGroupPackage.Types (PackageGroupPackage (..), PackageGroupPackageId (..))

getPackageGroupPackage :: DB :> es => PackageGroupPackageId -> Eff es (Maybe PackageGroupPackage)
getPackageGroupPackage packageGroupPackageId = dbtToEff $ selectById @PackageGroupPackage (Only packageGroupPackageId)
