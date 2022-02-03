{-# LANGUAGE QuasiQuotes #-}
module Flora.Model.User.Query where

import Control.Monad.IO.Class
import Data.Text (Text)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Transact (DBT)

import Data.Vector (Vector)
import qualified Database.PostgreSQL.Entity.DBT as DBT
import Flora.Model.User

getUserById :: (MonadIO m) => UserId -> DBT m (Maybe User)
getUserById userId = selectById (Only userId)

getUserByUsername :: (MonadIO m) => Text -> DBT m (Maybe User)
getUserByUsername username = selectOneByField [field| username |] (Only username)

getUserByEmail :: (MonadIO m) => Text -> DBT m (Maybe User)
getUserByEmail email = selectOneByField [field| email |] (Only email)

getAllUsers :: (MonadIO m) => DBT m (Vector User)
getAllUsers = DBT.query_ DBT.Select (_select @User)
