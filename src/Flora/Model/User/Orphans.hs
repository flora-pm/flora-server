{-# OPTIONS_GHC -Wno-orphans #-}
module Flora.Model.User.Orphans where

import Data.Password.Argon2 (Password, unsafeShowPassword)
import Data.Password.Types (mkPassword)
import Web.HttpApiData

instance FromHttpApiData Password where
  parseUrlPiece = Right . mkPassword

instance ToHttpApiData Password where
  toUrlPiece = unsafeShowPassword
