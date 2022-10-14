{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Orphans where

import Data.Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Function (on)
import Data.Text qualified as Text
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
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Distribution.Compiler (CompilerFlavor)
import Distribution.PackageDescription (FlagName)
import Distribution.Parsec
import Distribution.Pretty qualified as Pretty
import Distribution.SPDX.License qualified as SPDX
import Distribution.System (Arch, OS)
import Distribution.Types.Condition
import Distribution.Types.ConfVar
import Distribution.Types.Flag (PackageFlag (..))
import Distribution.Types.UnqualComponentName (UnqualComponentName, unUnqualComponentName)
import Distribution.Types.Version qualified as Cabal
import Distribution.Utils.Generic (fromUTF8BS)
import Distribution.Utils.ShortText
import Distribution.Version (Version, VersionRange)
import Servant (FromHttpApiData (..), ToHttpApiData (..))

deriving anyclass instance ToJSON ConfVar
deriving anyclass instance FromJSON ConfVar

deriving anyclass instance ToJSON t => ToJSON (Condition t)
deriving anyclass instance FromJSON t => FromJSON (Condition t)

deriving anyclass instance ToJSON OS
deriving anyclass instance FromJSON OS

deriving anyclass instance ToJSON Arch
deriving anyclass instance FromJSON Arch

instance ToJSON Version where
  toEncoding = Aeson.string . Pretty.prettyShow

instance FromJSON Version where
  parseJSON = withText "Version" $ \s ->
    maybe (fail "Invalid Version") pure (simpleParsec $ Text.unpack s)

instance FromField Version where
  fromField :: Field -> Maybe ByteString -> Conversion Version
  fromField f mdata = Cabal.mkVersion . fromPGArray <$> fromField f mdata

instance ToField Version where
  toField = toField . PGArray . Cabal.versionNumbers

instance Display Version where
  displayBuilder = Builder.fromString . Pretty.prettyShow

instance ToHttpApiData Version where
  toUrlPiece = Text.pack . Pretty.prettyShow

instance FromHttpApiData Version where
  parseUrlPiece piece =
    case simpleParsec $ Text.unpack piece of
      Nothing -> Left $ "Could not parse version string: " <> piece
      Just a -> Right a

deriving anyclass instance ToJSON VersionRange
deriving anyclass instance FromJSON VersionRange

deriving anyclass instance ToJSON CompilerFlavor
deriving anyclass instance FromJSON CompilerFlavor

deriving anyclass instance ToJSON FlagName
deriving anyclass instance FromJSON FlagName

instance ToJSON ShortText where
  toJSON = toJSON . fromShortText

instance FromJSON ShortText where
  parseJSON = fmap toShortText . parseJSON

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

instance Ord PackageFlag where
  compare = compare `on` flagName

deriving instance ToJSON PackageFlag
deriving instance FromJSON PackageFlag
