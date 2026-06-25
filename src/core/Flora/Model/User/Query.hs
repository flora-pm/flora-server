{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.User.Query where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (Only))
import Effectful

import Flora.Database
import Flora.Model.User

getUserById :: (IOE :> es, ReadDB :> es) => UserId -> Eff es (Maybe User)
getUserById userId = queryOne (_selectWhere @User [primaryKey @User]) (Only userId)

getUserByUsername :: (IOE :> es, ReadDB :> es) => Text -> Eff es (Maybe User)
getUserByUsername username = queryOne (_selectWhere @User [[field| username |]]) (Only username)

getUserByEmail :: (IOE :> es, ReadDB :> es) => Text -> Eff es (Maybe User)
getUserByEmail email = queryOne (_selectWhere @User [[field| email |]]) (Only email)

getAllUsers :: (IOE :> es, ReadDB :> es) => Eff es (Vector User)
getAllUsers = Vector.fromList <$> query_ (_select @User)
