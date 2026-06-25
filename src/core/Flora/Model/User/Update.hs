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
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Sel.HMAC.SHA256 qualified as HMAC

import Flora.Database
import Flora.Environment.Env
import Flora.Model.User
import Flora.Monad

addAdmin :: (IOE :> es, Reader FloraEnv :> es, Time :> es) => AdminCreationForm -> FloraM es User
addAdmin form = do
  FloraEnv{pool} <- Reader.ask
  adminUser <- mkAdmin form
  withReadWritePool pool $ do
    insertUser adminUser
    unlockAccount adminUser.userId
  pure adminUser

lockAccount :: (IOE :> es, Time :> es, WriteDB :> es) => UserId -> FloraM es ()
lockAccount userId = do
  ts <- Time.currentTime
  void $ execute q (ts, userId)
  where
    q =
      [sql|
        update users as u
        set user_flags = jsonb_set(user_flags, '{can_login}', 'false', false),
            updated_at = ?
        where u.user_id = ?;
      |]

unlockAccount :: (IOE :> es, Time :> es, WriteDB :> es) => UserId -> FloraM es ()
unlockAccount userId = do
  ts <- Time.currentTime
  void $ execute q (ts, userId)
  where
    q =
      [sql|
        update users as u
        set user_flags = jsonb_set(user_flags, '{can_login}', 'true', false),
            updated_at = ?
        where u.user_id = ?
      |]

insertUser :: (IOE :> es, WriteDB :> es) => User -> FloraM es ()
insertUser user = void $ execute (_insert @User) user

deleteUser :: (IOE :> es, WriteDB :> es) => UserId -> FloraM es ()
deleteUser userId = void $ execute (_delete @User) (Only userId)

setupTOTP
  :: (IOE :> es, Time :> es, WriteDB :> es)
  => UserId
  -> HMAC.AuthenticationKey
  -> FloraM es ()
setupTOTP userId key = do
  ts <- Time.currentTime
  void $ execute q (key, ts, userId)
  where
    q =
      [sql|
        update users as u
        set totp_key = ?,
            updated_at = ?
        where u.user_id = ?;
      |]

confirmTOTP
  :: (IOE :> es, Time :> es, WriteDB :> es)
  => UserId
  -> FloraM es ()
confirmTOTP userId = do
  ts <- Time.currentTime
  void $ execute q (ts, userId)
  where
    q =
      [sql|
        update users as u
        set totp_enabled = true,
            updated_at = ?
        where u.user_id = ?;
      |]

unSetTOTP
  :: (IOE :> es, Time :> es, WriteDB :> es)
  => UserId
  -> FloraM es ()
unSetTOTP userId = do
  ts <- Time.currentTime
  void $ execute q (ts, userId)
  where
    q =
      [sql|
        update users as u
        set totp_enabled = false,
            totp_key = Null,
            updated_at = ?
        where u.user_id = ?;
      |]
