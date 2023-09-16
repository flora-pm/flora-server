{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Orphans.Version where

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.Lazy.Builder qualified as Builder
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Distribution.Parsec
import Distribution.Pretty qualified as Pretty
import Distribution.Types.Version
import Distribution.Types.Version qualified as Cabal
import Distribution.Version (VersionRange)
import Servant

instance ToJSON Version where
  toJSON = Aeson.String . display . Pretty.prettyShow

instance FromJSON Version where
  parseJSON =
    withText
      "Version"
      ( \s ->
          maybe (fail "Invalid Version") pure (simpleParsec $ Text.unpack s)
      )

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

instance Display VersionRange where
  displayBuilder = Builder.fromString . Pretty.prettyShow

instance ToJSON VersionRange where
  toJSON = toJSON . Pretty.prettyShow

instance FromJSON VersionRange where
  parseJSON =
    withText
      "Version Range"
      ( \s ->
          maybe (fail "Invalid version range") pure (simpleParsec $ Text.unpack s)
      )
