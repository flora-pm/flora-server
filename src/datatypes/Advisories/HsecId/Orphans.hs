{-# OPTIONS_GHC -Wno-orphans #-}

module Advisories.HsecId.Orphans where

import Control.DeepSeq
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.Encoding qualified as Text
import Database.PostgreSQL.Simple.FromField (FromField (..), ResultError (..), returnError)
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))
import Security.Advisories.Core.HsecId

deriving via ShowInstance HsecId instance Display HsecId

instance ToField HsecId where
  toField = Escape . Text.encodeUtf8 . display

instance FromField HsecId where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField _ (Just bs) | Just hsecId <- parseHsecId (Text.unpack $ Text.decodeUtf8 bs) = pure hsecId
  fromField f (Just bs) =
    returnError ConversionFailed f $
      Text.unpack $
        "Conversion error: Expected parseable HsecId but instead got "
          <> Text.decodeUtf8 bs

instance NFData HsecId where
  rnf a = seq a ()
