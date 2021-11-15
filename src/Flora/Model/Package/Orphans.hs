{-# OPTIONS_GHC -fno-warn-orphans #-}
module Flora.Model.Package.Orphans where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromField (Conversion, Field, FromField (..),
                                             ResultError (ConversionFailed, UnexpectedNull),
                                             returnError)
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))
import Distribution.Parsec
import qualified Distribution.Pretty as Pretty
import qualified Distribution.SPDX.License as SPDX
import Distribution.Simple.Utils (fromUTF8BS)

instance FromField SPDX.License where
  fromField :: Field -> Maybe ByteString -> Conversion SPDX.License
  fromField f mdata =
    case mdata of
        Nothing -> returnError UnexpectedNull f ""
        Just bs ->
          case simpleParsec (fromUTF8BS bs) of
              Just (a :: SPDX.License) -> pure a
              Nothing -> returnError ConversionFailed f $
                "Conversion error: Expected valid SPDX identifier for 'license', got: " <> fromUTF8BS bs

instance ToField SPDX.License where
  toField = Escape . C8.pack . Pretty.prettyShow


instance FromJSON SPDX.License where
  parseJSON = withText "SPDX License" $ \s ->
    maybe (fail "Invalid SPDX License expression!") pure (simpleParsec $ T.unpack s)

instance ToJSON SPDX.License where
  toJSON = String . T.pack . Pretty.prettyShow
