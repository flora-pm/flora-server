module FloraWeb.Common.Auth.TwoFactor
  ( uriFromKey
  , validateTOTP
  ) where

import Chronos (Timespan, now, second)
import Data.ByteString.Base32 qualified as Base32
import Data.Maybe (fromJust)
import Data.Text (Text)
import OTP.Commons
import OTP.TOTP
import Sel.HMAC.SHA256 qualified as HMAC
import Torsor (scale)

period :: Timespan
period = scale 30 second

sixDigits :: Digits
sixDigits = fromJust $ mkDigits 6

uriFromKey :: Text -> Text -> HMAC.AuthenticationKey -> Text
uriFromKey domain email key =
  let
    issuer = "Flora (" <> domain <> ")"
   in
    totpToURI
      (Base32.encodeBase32Unpadded $ HMAC.unsafeAuthenticationKeyToBinary key)
      email
      issuer
      sixDigits
      period
      HMAC_SHA1

validateTOTP :: HMAC.AuthenticationKey -> Text -> IO Bool
validateTOTP key code = do
  timestamp <- now
  pure $
    totpSHA1Check
      key
      (1, 1)
      timestamp
      period
      sixDigits
      code
