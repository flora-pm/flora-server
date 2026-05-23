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
import Effectful (Eff, IOE, type (:>))
import Effectful.Labeled
import Effectful.PostgreSQL
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Sel.HMAC.SHA256 qualified as HMAC

import Flora.Database
import Flora.Environment.Env
import Flora.Model.User

addAdmin :: (IOE :> es, Reader FloraEnv :> es, Time :> es) => AdminCreationForm -> Eff es User
addAdmin form = do
  FloraEnv{pool} <- Reader.ask
  adminUser <- mkAdmin form
  withReadWritePool pool $ insertUser adminUser
  withReadWritePool pool $ unlockAccount adminUser.userId
  pure adminUser

lockAccount :: (IOE :> es, Labeled ReadWrite WithConnection :> es, Time :> es) => UserId -> Eff es ()
lockAccount userId = do
  ts <- Time.currentTime
  labeled @ReadWrite @WithConnection $ void $ execute q (ts, userId)
  where
    q =
      [sql|
        update users as u
        set user_flags = jsonb_set(user_flags, '{can_login}', 'false', false),
            updated_at = ?
        where u.user_id = ?;
      |]

unlockAccount :: (IOE :> es, Labeled ReadWrite WithConnection :> es, Time :> es) => UserId -> Eff es ()
unlockAccount userId = do
  ts <- Time.currentTime
  labeled @ReadWrite @WithConnection $ void $ execute q (ts, userId)
  where
    q =
      [sql|
        update users as u
        set user_flags = jsonb_set(user_flags, '{can_login}', 'true', false),
            updated_at = ?
        where u.user_id = ?
      |]

insertUser :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => User -> Eff es ()
insertUser user = labeled @ReadWrite @WithConnection $ void $ execute (_insert @User) user

deleteUser :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => UserId -> Eff es ()
deleteUser userId = labeled @ReadWrite @WithConnection $ void $ execute (_delete @User) (Only userId)

setupTOTP
  :: (IOE :> es, Labeled ReadWrite WithConnection :> es, Time :> es)
  => UserId
  -> HMAC.AuthenticationKey
  -> Eff es ()
setupTOTP userId key = do
  ts <- Time.currentTime
  labeled @ReadWrite @WithConnection $ void $ execute q (key, ts, userId)
  where
    q =
      [sql|
        update users as u
        set totp_key = ?,
            updated_at = ?
        where u.user_id = ?;
      |]

confirmTOTP
  :: (IOE :> es, Labeled ReadWrite WithConnection :> es, Time :> es)
  => UserId
  -> Eff es ()
confirmTOTP userId = do
  ts <- Time.currentTime
  labeled @ReadWrite @WithConnection $ void $ execute q (ts, userId)
  where
    q =
      [sql|
        update users as u
        set totp_enabled = true,
            updated_at = ?
        where u.user_id = ?;
      |]

unSetTOTP
  :: (IOE :> es, Labeled ReadWrite WithConnection :> es, Time :> es)
  => UserId
  -> Eff es ()
unSetTOTP userId = do
  ts <- Time.currentTime
  labeled @ReadWrite @WithConnection $ void $ execute q (ts, userId)
  where
    q =
      [sql|
        update users as u
        set totp_enabled = false,
            totp_key = Null,
            updated_at = ?
        where u.user_id = ?;
      |]
