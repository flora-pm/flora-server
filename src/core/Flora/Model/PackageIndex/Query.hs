{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageIndex.Query where

import Data.Text (Text)
import Database.PostgreSQL.Entity (selectOneByField)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (..))
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Flora.Model.PackageIndex.Types

getPackageIndexByName :: DB :> es => Text -> Eff es (Maybe PackageIndex)
getPackageIndexByName repository =
  let index = case repository of
        "haskell" -> "hackage"
        r -> r
   in dbtToEff $
        selectOneByField [field| repository |] (Only index)
