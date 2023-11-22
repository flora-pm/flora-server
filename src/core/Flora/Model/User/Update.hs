{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.User.Update
  ( addAdmin
  , lockAccount
  , unlockAccount
  , insertUser
  , deleteUser
  , setupTOTP
  , confirmTOTP
  , unSetTOTP
  ) where

import Control.Monad
import Database.PostgreSQL.Entity (delete, insert)
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful (Eff, IOE, type (:>))
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Sel.HMAC.SHA256 qualified as HMAC

import Flora.Model.User

addAdmin :: (DB :> es, Time :> es, IOE :> es) => AdminCreationForm -> Eff es User
addAdmin form = do
  adminUser <- mkAdmin form
  insertUser adminUser
  unlockAccount adminUser.userId
  pure adminUser

lockAccount :: (DB :> es, Time :> es) => UserId -> Eff es ()
lockAccount userId = do
  ts <- Time.currentTime
  dbtToEff $ void $ execute Update q (ts, userId)
  where
    q =
      [sql|
        update users as u
        set user_flags = jsonb_set(user_flags, '{can_login}', 'false', false),
            updated_at = ?
        where u.user_id = ?;
      |]

unlockAccount :: (DB :> es, Time :> es) => UserId -> Eff es ()
unlockAccount userId = do
  ts <- Time.currentTime
  dbtToEff $ void $ execute Update q (ts, userId)
  where
    q =
      [sql|
        update users as u
        set user_flags = jsonb_set(user_flags, '{can_login}', 'true', false),
            updated_at = ?
        where u.user_id = ?
      |]

insertUser :: DB :> es => User -> Eff es ()
insertUser user = dbtToEff $ insert @User user

deleteUser :: DB :> es => UserId -> Eff es ()
deleteUser userId = dbtToEff $ delete @User (Only userId)

setupTOTP
  :: (DB :> es, Time :> es)
  => UserId
  -> HMAC.AuthenticationKey
  -> Eff es ()
setupTOTP userId key = do
  ts <- Time.currentTime
  dbtToEff $ void $ execute Update q (key, ts, userId)
  where
    q =
      [sql|
        update users as u
        set totp_key = ?,
            updated_at = ?
        where u.user_id = ?;
      |]

confirmTOTP
  :: (DB :> es, Time :> es)
  => UserId
  -> Eff es ()
confirmTOTP userId = do
  ts <- Time.currentTime
  dbtToEff $ void $ execute Update q (ts, userId)
  where
    q =
      [sql|
        update users as u
        set totp_active = true,
            updated_at = ?
        where u.user_id = ?;
      |]

unSetTOTP
  :: (DB :> es, Time :> es)
  => UserId
  -> Eff es ()
unSetTOTP userId = do
  ts <- Time.currentTime
  dbtToEff $ void $ execute Update q (ts, userId)
  where
    q =
      [sql|
        update users as u
        set totp_enabled = false,
            totp_key = Null,
            updated_at = ?
        where u.user_id = ?;
      |]
