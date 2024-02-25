module Flora.QRCode where

import Codec.Picture
import Codec.QRCode
import Codec.QRCode.JuicyPixels
import Data.ByteString (StrictByteString)
import Data.ByteString.Base64
import Data.ByteString.Lazy qualified as BSL
import Data.Function ((&))
import Data.Text (Text)
import Data.Base64.Types (extractBase64)

generateQRCode :: Text -> StrictByteString
generateQRCode uri =
  case encodeText (defaultQRCodeOptions L) Iso8859_1OrUtf8WithoutECI uri of
    Nothing -> error $ "QR code can't be encoded for text " <> show uri
    Just qrImage ->
      toImage 4 20 qrImage
        & encodePng
        & BSL.toStrict
        & encodeBase64'
        & extractBase64
