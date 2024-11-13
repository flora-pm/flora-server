{-# OPTIONS_GHC -Wno-orphans #-}

module Advisories.CVSS.Orphans where

import Control.DeepSeq
import Data.Text.Encoding qualified as Text
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Security.CVSS (CVSS)
import Security.CVSS qualified as CVSS

instance ToField CVSS where
  toField = toField . CVSS.cvssVectorStringOrdered

instance FromField CVSS where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField f (Just bs) =
    let field = Text.decodeUtf8 bs
     in case CVSS.parseCVSS field of
          Right cvss -> pure cvss
          Left err ->
            returnError ConversionFailed f $
              "Conversion error. Could not parse CVSS: " <> show err

instance NFData CVSS where
  rnf a = seq a ()
