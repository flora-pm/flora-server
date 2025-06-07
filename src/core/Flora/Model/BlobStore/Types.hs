module Flora.Model.BlobStore.Types where

import Control.DeepSeq (NFData)
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as B16
import Data.Text (Text)
import Data.Text.Builder.Linear (fromText)
import Data.Text.Display (Display (..), display)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import GHC.Generics (Generic)

newtype Sha256Sum = Sha256Sum {bytestring :: ByteString}
  deriving (Eq, Generic, Show)
  deriving newtype (NFData, Ord)

instance ToField Sha256Sum where
  toField = toField . decodeUtf8Lenient . B16.encode . bytestring

instance FromField Sha256Sum where
  fromField f mbs =
    Sha256Sum . B16.decodeLenient . encodeUtf8
      <$> fromField @Text f mbs

instance Display Sha256Sum where
  displayBuilder = fromText . decodeUtf8Lenient . B16.encode . bytestring

instance ToJSON Sha256Sum where
  toJSON = String . display

instance FromJSON Sha256Sum where
  parseJSON (String txt) = pure . Sha256Sum . B16.decodeLenient $ encodeUtf8 txt
  parseJSON invalid =
    prependFailure "Parsing Sha256Sum failed" $!
      typeMismatch "Invalid" invalid
