{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -Wno-redundant-constraints #-}

module Flora.Model.User
  ( UserId (..)
  , User (..)
  , UserFlags (..)
  , UserCreationForm (..)
  , AdminCreationForm (..)
  , mkUser
  , mkAdmin
  , hashPassword
  , validatePassword
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Password.Argon2
  ( Argon2
  , Password
  , PasswordCheck (PasswordCheckSuccess)
  , PasswordHash
  )
import Data.Password.Argon2 qualified as Argon2
import Data.Password.Orphans ()
import Data.Text (Text)
import Data.Text.Display (Display, ShowInstance (..), displayBuilder)
import Data.Time (UTCTime)
import Data.UUID
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple.FromField (FromField (..), fromJSONField)
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToField (ToField (..), toJSONField)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Effectful
import Effectful.Time qualified as Time
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

newtype UserId = UserId {getUserId :: UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, FromJSON, ToJSON, FromField, ToField, FromHttpApiData, ToHttpApiData, NFData)
    via UUID
  deriving
    (Display)
    via (ShowInstance UUID)

data User = User
  { userId :: UserId
  , username :: Text
  , email :: Text
  , displayName :: Text
  , password :: PasswordHash Argon2
  , userFlags :: UserFlags
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[TableName "users"] User)
  deriving
    (Display)
    via (ShowInstance User)

data UserFlags = UserFlags
  { isAdmin :: Bool
  , canLogin :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON, NFData)

instance FromField UserFlags where
  fromField = fromJSONField

instance ToField UserFlags where
  toField = toJSONField

data UserCreationForm = UserCreationForm
  { username :: Text
  , email :: Text
  , password :: PasswordHash Argon2
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data AdminCreationForm = AdminCreationForm
  { username :: Text
  , email :: Text
  , password :: PasswordHash Argon2
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Type error! Do not use 'toJSON' on a 'Password'!
instance TypeError (CannotDisplayPassword "JSON") => ToJSON Password where
  toJSON = error "unreachable"

-- | Type error! Do not use 'display' on a 'Password'!
instance TypeError (CannotDisplayPassword "Text") => Display Password where
  displayBuilder = error "unreachable"

type CannotDisplayPassword e =
  'Text "🚫 Tried to convert plain-text Password to "
    ':<>: 'Text e
    ':<>: 'Text "!"
    ':$$: 'Text "  This is likely a security leak. Please make sure whether this was intended."
    ':$$: 'Text "  If this is intended, please use 'unsafeShowPassword' before converting to "
    ':<>: 'Text e
    ':$$: 'Text ""

deriving via Text instance ToField (PasswordHash a)
deriving via Text instance FromField (PasswordHash a)

mkUser :: IOE :> es => UserCreationForm -> Eff es User
mkUser UserCreationForm{username, email, password} = do
  userId <- UserId <$> liftIO UUID.nextRandom
  timestamp <- liftIO Time.currentTime
  let createdAt = timestamp
  let updatedAt = timestamp
  let displayName = ""
  let userFlags = UserFlags{isAdmin = False, canLogin = True}
  pure User{..}

mkAdmin :: IOE :> es => AdminCreationForm -> Eff es User
mkAdmin AdminCreationForm{username, email, password} = do
  userId <- UserId <$> liftIO UUID.nextRandom
  timestamp <- liftIO Time.currentTime
  let createdAt = timestamp
  let updatedAt = timestamp
  let displayName = ""
  let userFlags = UserFlags{isAdmin = True, canLogin = False}
  pure User{..}

hashPassword :: IOE :> es => Password -> Eff es (PasswordHash Argon2)
hashPassword = Argon2.hashPassword

validatePassword :: Password -> PasswordHash Argon2 -> Bool
validatePassword inputPassword hashedPassword =
  Argon2.checkPassword inputPassword hashedPassword == PasswordCheckSuccess
