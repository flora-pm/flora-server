{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans -Wno-redundant-constraints #-}
module Flora.Model.User where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Password.Argon2
import qualified Data.Password.Argon2 as Argon2
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Transact (DBT)
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), TypeError)

newtype UserId = UserId { getUserId :: UUID }
  deriving stock (Generic, Show)
  deriving (Eq, Ord, FromJSON, ToJSON, FromField, ToField)
    via UUID

data User = User
  { userId      :: UserId
  , username    :: Text
  , email       :: Text
  , displayName :: Text
  , password    :: PasswordHash Argon2
  , createdAt   :: UTCTime
  , updatedAt   :: UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)
  deriving Entity
    via (GenericEntity '[TableName "users"] User)

-- | Type error! Do not use 'toJSON' on a 'Password'!
instance TypeError (CannotDisplayPassword "JSON") => ToJSON Password where
  toJSON = error "unreachable"

-- -- | Type error! Do not use 'display' on a 'Password'!
-- instance TypeError (CannotDisplayPassword "Text") => Display Password where
--   toJSON = error "unreachable"

type CannotDisplayPassword e =
  'Text "🚫 Tried to convert plain-text Password to " ':<>: 'Text e ':<>: 'Text "!"
  ':$$: 'Text "  This is likely a security leak. Please make sure whether this was intended."
  ':$$: 'Text "  If this is intended, please use 'unsafeShowPassword' before converting to "
  ':<>: 'Text e
  ':$$: 'Text ""

deriving via Text instance ToField (PasswordHash a)
deriving via Text instance FromField (PasswordHash a)

hashPassword :: (MonadIO m) => Password -> m (PasswordHash Argon2)
hashPassword = Argon2.hashPassword

validatePassword :: Password -> PasswordHash Argon2 -> Bool
validatePassword inputPassword hashedPassword =
  Argon2.checkPassword inputPassword hashedPassword == PasswordCheckSuccess

insertUser :: (MonadIO m) => User -> DBT m ()
insertUser user = insert @User user

getUserById :: (MonadIO m) => UserId -> DBT m (Maybe User)
getUserById userId = selectById (Only userId)

getUserByUsername :: (MonadIO m) => Text -> DBT m (Maybe User)
getUserByUsername username = selectOneByField [field| username |] (Only username)

getUserByEmail :: (MonadIO m) => Text -> DBT m (Maybe User)
getUserByEmail email = selectOneByField [field| email |] (Only email)

deleteUser :: (MonadIO m) => UserId -> DBT m ()
deleteUser userId = delete @User (Only userId)
