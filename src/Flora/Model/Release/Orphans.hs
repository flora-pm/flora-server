{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flora.Model.Release.Orphans where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as Text
import Data.Text.Display
import qualified Data.Text.Lazy.Builder as Builder
import Database.PostgreSQL.Simple.FromField
  ( Conversion
  , Field
  , FromField (..)
  , ResultError (ConversionFailed, UnexpectedNull)
  , returnError
  )
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Distribution.Parsec
import Distribution.Pretty (prettyShow)
import qualified Distribution.Pretty as Pretty
import qualified Distribution.SPDX.License as SPDX
import Distribution.Simple.Utils (fromUTF8BS)
import Distribution.Types.Version
import qualified Distribution.Types.Version as Cabal
import Servant (FromHttpApiData (..), ToHttpApiData (..))

instance FromField Version where
  fromField :: Field -> Maybe ByteString -> Conversion Version
  fromField f mdata = Cabal.mkVersion . fromPGArray <$> fromField f mdata

instance ToHttpApiData Version where
  toUrlPiece = Text.pack . Pretty.prettyShow

instance FromHttpApiData Version where
  parseUrlPiece piece =
    case simpleParsec $ Text.unpack piece of
      Nothing -> Left $ "Could not parse version string: " <> piece
      Just a -> Right a

instance ToField Version where
  toField = toField . PGArray . Cabal.versionNumbers

instance Display Version where
  displayBuilder = Builder.fromString . prettyShow

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
