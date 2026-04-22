module Flora.Model.PackageUploader.Update where

import Database.PostgreSQL.Entity (insert)
import Effectful (Eff, type (:>))
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Flora.Model.PackageUploader.Types

insertPackageUploader
  :: DB :> es
  => PackageUploaderDAO
  -> Eff es ()
insertPackageUploader packageUploader =
  dbtToEff $
    insert @PackageUploaderDAO packageUploader
