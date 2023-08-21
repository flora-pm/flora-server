{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Orphans where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Text.Display
import Data.Text.Lazy.Builder qualified as Builder
import Database.PostgreSQL.Simple.FromField
  ( Conversion
  , Field
  , FromField (..)
  , ResultError (ConversionFailed, UnexpectedNull)
  , returnError
  )
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))
import Distribution.Parsec
import Distribution.Pretty qualified as Pretty
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.UnqualComponentName (UnqualComponentName, unUnqualComponentName)
import Distribution.Utils.Generic (fromUTF8BS)

instance FromField SPDX.License where
  fromField :: Field -> Maybe ByteString -> Conversion SPDX.License
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs ->
        case simpleParsec (fromUTF8BS bs) of
          Just (a :: SPDX.License) -> pure a
          Nothing ->
            returnError ConversionFailed f $
              "Conversion error: Expected valid SPDX identifier for 'license', got: " <> fromUTF8BS bs

instance ToField SPDX.License where
  toField = Escape . C8.pack . Pretty.prettyShow

instance Display UnqualComponentName where
  displayBuilder = Builder.fromString . unUnqualComponentName
