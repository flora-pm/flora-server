{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -Wno-unused-imports #-}

module Flora.Model.User
  ( UserId (..)
  , User (..)
  , UserFlags (..)
  , UserCreationForm (..)
  , AdminCreationForm (..)
  , mkUser
  , mkAdmin
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text)
import Data.Text.Display (Display, ShowInstance (..))
import Data.Time (UTCTime)
import Data.UUID
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Binary (..))
import Database.PostgreSQL.Simple.FromField (FromField (..), ResultError (..), fromJSONField, returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToField (ToField (..), toJSONField)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Effectful
import Effectful.Time qualified as Time
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Sel.HMAC.SHA256 qualified as HMAC
import Sel.Hashing.Password
import Sel.Hashing.Password qualified as Sel
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

import Database.PostgreSQL.Simple.Orphans ()

newtype UserId = UserId {getUserId :: UUID}
  deriving stock (Generic, Show)
  deriving
    (Display)
    via (ShowInstance UUID)
  deriving
    (Eq, FromField, FromHttpApiData, FromJSON, NFData, Ord, ToField, ToHttpApiData, ToJSON)
    via UUID

data User = User
  { userId :: UserId
  , username :: Text
  , email :: Text
  , displayName :: Text
  , password :: PasswordHash
  , userFlags :: UserFlags
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , totpKey :: Maybe HMAC.AuthenticationKey
  , totpEnabled :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
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
  deriving anyclass (FromJSON, NFData, ToJSON)

instance FromField UserFlags where
  fromField = fromJSONField

instance ToField UserFlags where
  toField = toJSONField

data UserCreationForm = UserCreationForm
  { username :: Text
  , email :: Text
  , password :: PasswordHash
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data AdminCreationForm = AdminCreationForm
  { username :: Text
  , email :: Text
  , password :: PasswordHash
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Type error! Do not use 'toJSON' on a 'Password'!
instance TypeError (CannotDisplayPassword "JSON") => ToJSON PasswordHash where
  toJSON = error "unreachable"

type CannotDisplayPassword e =
  'Text "🚫 Tried to convert plain-text Password to "
    ':<>: 'Text e
    ':<>: 'Text "!"
    ':$$: 'Text "  This is likely a security leak. Please make sure whether this was intended."
    ':$$: 'Text "  If this is intended, please use 'unsafeShowPassword' before converting to "
    ':<>: 'Text e
    ':$$: 'Text ""

instance ToField PasswordHash where
  toField = toField . Sel.passwordHashToText

instance FromField PasswordHash where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField _ (Just bs) =
    pure $ Sel.asciiByteStringToPasswordHash bs

instance NFData PasswordHash where
  rnf a = seq a ()

mkUser :: IOE :> es => UserCreationForm -> Eff es User
mkUser UserCreationForm{username, email, password} = do
  userId <- UserId <$> liftIO UUID.nextRandom
  timestamp <- liftIO Time.currentTime
  let createdAt = timestamp
  let updatedAt = timestamp
  let displayName = ""
  let userFlags = UserFlags{isAdmin = False, canLogin = True}
  let totpKey = Nothing
  let totpEnabled = False
  pure User{userId, username, email, displayName, password, userFlags, createdAt, updatedAt, totpKey, totpEnabled}

mkAdmin :: IOE :> es => AdminCreationForm -> Eff es User
mkAdmin AdminCreationForm{username, email, password} = do
  userId <- UserId <$> liftIO UUID.nextRandom
  timestamp <- liftIO Time.currentTime
  let createdAt = timestamp
  let updatedAt = timestamp
  let displayName = ""
  let userFlags = UserFlags{isAdmin = True, canLogin = True}
  let totpKey = Nothing
  let totpEnabled = False
  pure User{userId, username, email, displayName, password, userFlags, createdAt, updatedAt, totpKey, totpEnabled}
