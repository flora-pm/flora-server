{-# OPTIONS_GHC -fno-warn-orphans #-}
module Flora.Model.Package.Orphans where

import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple.FromField (Conversion, Field, FromField (..), returnError, ResultError (ConversionFailed, UnexpectedNull))
import Database.PostgreSQL.Simple.ToField (ToField (..), Action(..))    
import Distribution.Parsec
import Distribution.Simple.Utils (fromUTF8BS)
import qualified Data.ByteString.Char8 as C8
import qualified Distribution.Pretty as Pretty
import qualified Distribution.SPDX.License as SPDX

instance FromField SPDX.License where
  fromField :: Field -> Maybe ByteString -> Conversion SPDX.License
  fromField f mdata =
    case mdata of
        Nothing -> returnError UnexpectedNull f ""
        Just bs ->
          case eitherParsec (fromUTF8BS bs) of
              Right (a :: SPDX.License) -> pure a
              Left _ -> returnError ConversionFailed f $
                "Conversion error: Expected valid SPDX identifier for 'license', got: " <> fromUTF8BS bs

instance ToField SPDX.License where
  toField = Escape . C8.pack . Pretty.prettyShow 
