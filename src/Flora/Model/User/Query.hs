{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.User.Query where

import Data.Text (Text)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (Only))

import Data.Vector (Vector)
import qualified Database.PostgreSQL.Entity.DBT as DBT
import Effectful
import Effectful.PostgreSQL.Transact.Effect
import Flora.Model.User

getUserById :: (DB :> es, IOE :> es) => UserId -> Eff es (Maybe User)
getUserById userId = dbtToEff $ selectById (Only userId)

getUserByUsername :: ([DB, IOE] :>> es) => Text -> Eff es (Maybe User)
getUserByUsername username = dbtToEff $ selectOneByField [field| username |] (Only username)

getUserByEmail :: ([DB, IOE] :>> es) => Text -> Eff es (Maybe User)
getUserByEmail email = dbtToEff $ selectOneByField [field| email |] (Only email)

getAllUsers :: ([DB, IOE] :>> es) => Eff es (Vector User)
getAllUsers = dbtToEff $ DBT.query_ DBT.Select (_select @User)
