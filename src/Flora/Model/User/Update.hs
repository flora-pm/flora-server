{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.User.Update where

import Control.Monad
import Database.PostgreSQL.Entity (delete, insert)
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Effectful (Eff, IOE, type (:>>))
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Flora.Model.User
import Optics.Core ((^.))

addAdmin :: ([DB, IOE] :>> es) => AdminCreationForm -> Eff es User
addAdmin form = do
  adminUser <- mkAdmin form
  insertUser adminUser
  unlockAccount (adminUser ^. #userId)
  pure adminUser

lockAccount :: ([DB, IOE] :>> es) => UserId -> Eff es ()
lockAccount userId = dbtToEff $ void $ execute Update q (Only userId)
  where
    q =
      [sql|
        update users as u set user_flags = jsonb_set(user_flags, '{canLogin}', 'false', false)
        where u.user_id = ?;
      |]

unlockAccount :: ([DB, IOE] :>> es) => UserId -> Eff es ()
unlockAccount userId = dbtToEff $ void $ execute Update q (Only userId)
  where
    q =
      [sql|
        update users as u set user_flags = jsonb_set(user_flags, '{canLogin}', 'true', false)
        where u.user_id = ?;
      |]

insertUser :: ([DB, IOE] :>> es) => User -> Eff es ()
insertUser user = dbtToEff $ insert @User user

deleteUser :: ([DB, IOE] :>> es) => UserId -> Eff es ()
deleteUser userId = dbtToEff $ delete @User (Only userId)
