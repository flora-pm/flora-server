{-# OPTIONS_GHC -Wno-orphans #-}

module Flora.Model.User.Orphans where

import Data.Password.Argon2 (Password, PasswordHash, unsafeShowPassword)
import Data.Password.Types (mkPassword)
import Data.Text.Display (Display (..))
import Web.HttpApiData

instance FromHttpApiData Password where
  parseUrlPiece = Right . mkPassword

instance ToHttpApiData Password where
  toUrlPiece = unsafeShowPassword

instance Display (PasswordHash a) where
  displayBuilder _ = "[REDACTED]"
