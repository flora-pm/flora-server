{-# OPTIONS_GHC -Wno-orphans #-}

module Database.PostgreSQL.Simple.Orphans where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import Database.PostgreSQL.Simple.FromField (FromField (..), ResultError (..), returnError)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types (Binary (..))
import Sel.HMAC.SHA256 qualified as HMAC

deriving newtype instance NFData (Binary ByteString)

instance FromField HMAC.AuthenticationKey where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField f (Just bs) =
    case HMAC.authenticationKeyFromHexByteString bs of
      Left err -> returnError ConversionFailed f (Text.unpack err)
      Right value -> pure value

instance ToField HMAC.AuthenticationKey where
  toField = Escape . HMAC.unsafeAuthenticationKeyToHexByteString

instance NFData HMAC.AuthenticationKey where
  rnf :: HMAC.AuthenticationKey -> ()
  rnf a = seq a ()
