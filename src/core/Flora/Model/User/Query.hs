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
import Effectful.Labeled
import Effectful.PostgreSQL

import Flora.Database
import Flora.Model.User

getUserById :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => UserId -> Eff es (Maybe User)
getUserById userId = labeled @ReadOnly @WithConnection $ queryOne (_selectWhere @User [primaryKey @User]) (Only userId)

getUserByUsername :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => Text -> Eff es (Maybe User)
getUserByUsername username = labeled @ReadOnly @WithConnection $ queryOne (_selectWhere @User [[field| username |]]) (Only username)

getUserByEmail :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => Text -> Eff es (Maybe User)
getUserByEmail email = labeled @ReadOnly @WithConnection $ queryOne (_selectWhere @User [[field| email |]]) (Only email)

getAllUsers :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => Eff es (Vector User)
getAllUsers = labeled @ReadOnly @WithConnection $ Vector.fromList <$> query_ (_select @User)
