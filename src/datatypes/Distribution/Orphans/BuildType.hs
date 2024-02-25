{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Orphans.BuildType where

import Data.Aeson (camelTo2)
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Distribution.Parsec
import Distribution.Pretty qualified as Pretty
import Distribution.Types.BuildType (BuildType (..))
import Distribution.Utils.Generic (fromUTF8BS)

instance FromField BuildType where
  fromField :: Field -> Maybe ByteString -> Conversion BuildType
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs ->
        case simpleParsec (fromUTF8BS bs) of
          Just (a :: BuildType) -> pure a
          Nothing ->
            returnError ConversionFailed f $
              "Conversion error: Expected valid Build Type for 'build_type', got: " <> fromUTF8BS bs

instance ToField BuildType where
  toField = Escape . C8.pack . Pretty.prettyShow

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''BuildType)
