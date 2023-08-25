module Flora.Model.BlobStore.Types where

import Control.DeepSeq (NFData)
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL
import Data.Text.Display (Display (..), display)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Builder (fromText)
import GHC.Generics (Generic)

import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.Types (Binary (..))

-- Helper type for displaying bytestring as base64 encoded utf8
newtype B64ByteString = B64ByteString ByteString
  deriving (Generic)
  deriving newtype (ToField, FromField)

instance Display B64ByteString where
  displayBuilder (B64ByteString bs) = fromText . encodeBase64 $ bs

instance ToJSON B64ByteString where
  toJSON bs = String $ display bs

instance FromJSON B64ByteString where
  parseJSON (String txt) =
    pure $!
      B64ByteString $!
        decodeBase64Lenient $!
          encodeUtf8 txt
  parseJSON invalid =
    prependFailure "Parsing B64ByteString failed" $!
      typeMismatch "Invalid" invalid

newtype Sha256Sum = Sha256Sum {bytestring :: ByteString}
  deriving (Eq, Show, Generic)
  deriving newtype (Ord, NFData)
  -- Binary ensures it doesn't try and do this via utf-8
  deriving (ToField, FromField) via Binary ByteString
  -- Display this in base 64
  deriving (Display, FromJSON, ToJSON) via B64ByteString
