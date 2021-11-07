{-# OPTIONS_GHC -fno-warn-orphans #-}
module Flora.Model.Release.Orphans where

import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple.FromField (Conversion, Field, FromField (..), returnError, ResultError (ConversionFailed, UnexpectedNull))
import Database.PostgreSQL.Simple.ToField (ToField (..), Action(..))    
import Distribution.Parsec
import Distribution.Simple.Utils (fromUTF8BS)
import Distribution.Types.Version
import Distribution.Types.VersionRange
import qualified Data.ByteString.Char8 as C8
import qualified Distribution.Pretty as Pretty

instance FromField Version where
  fromField :: Field -> Maybe ByteString -> Conversion Version
  fromField f mdata =
    case mdata of
        Nothing -> returnError UnexpectedNull f ""
        Just bs ->
          case simpleParsec (fromUTF8BS bs) of
              Just (a :: Version) -> pure a
              Nothing -> returnError ConversionFailed f $
                "Conversion error: Expected valid version expression, got: " <> fromUTF8BS bs

instance ToField Version where
  toField = Escape . C8.pack . Pretty.prettyShow 

instance FromField VersionRange where
  fromField :: Field -> Maybe ByteString -> Conversion VersionRange
  fromField f mdata =
    case mdata of
        Nothing -> returnError UnexpectedNull f ""
        Just bs ->
          case simpleParsec (fromUTF8BS bs) of
              Just (a :: VersionRange) -> pure a
              Nothing -> returnError ConversionFailed f $
                "Conversion error: Expected valid version range, got: " <> fromUTF8BS bs

instance ToField VersionRange where
  toField = Escape . C8.pack . Pretty.prettyShow 
