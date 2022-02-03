{-# LANGUAGE QuasiQuotes #-}
module Flora.Model.User.Update where

import Control.Monad
import Control.Monad.IO.Class
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)

import Flora.Model.User
import Optics.Core

addAdmin :: (MonadIO m) => AdminCreationForm -> DBT m User
addAdmin form = do
  adminUser <- mkAdmin form
  insertUser adminUser
  unlockAccount (adminUser ^. #userId)
  pure adminUser

lockAccount :: (MonadIO m) => UserId -> DBT m ()
lockAccount userId = void $ execute Update q (Only userId)
  where
    q = [sql|
        update users as u set user_flags = jsonb_set(user_flags, '{canLogin}', 'false', false)
        where u.user_id = ?;
      |]

unlockAccount :: (MonadIO m) => UserId -> DBT m ()
unlockAccount userId = void $ execute Update q (Only userId)
  where
    q = [sql|
        update users as u set user_flags = jsonb_set(user_flags, '{canLogin}', 'true', false)
        where u.user_id = ?;
      |]

insertUser :: (MonadIO m) => User -> DBT m ()
insertUser user = insert @User user

deleteUser :: (MonadIO m) => UserId -> DBT m ()
deleteUser userId = delete @User (Only userId)
