{-# LANGUAGE TemplateHaskell #-}

module Flora.Model.Release.Types
  ( ReleaseId (..)
  , TextHtml (..)
  , Release (..)
  , ImportStatus (..)
  , SupportedCompilers (..)
  , ReleaseDeprecation (..)
  , ReleaseFlags (..)
  )
where

import Data.Aeson
import Data.Aeson.Orphans ()
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.OpenApi.Schema (ToSchema)
import Data.Text (Text, unpack)
import Data.Text.Display
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.Types (Entity, GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (..), ResultError (..), returnError)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))
import Distribution.Compiler (CompilerFlavor)
import Distribution.SPDX.License ()
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.Flag (PackageFlag)
import Distribution.Types.Version
import Distribution.Types.VersionRange (VersionRange)
import Lucid qualified

import Control.DeepSeq
import Data.Text.Lazy qualified as Text
import Deriving.Aeson
import Distribution.Orphans ()
import Distribution.Orphans.CompilerFlavor ()
import Distribution.Orphans.PackageFlag ()
import Flora.Model.Package

newtype ReleaseId = ReleaseId {getReleaseId :: UUID}
  deriving
    (Eq, Ord, Show, FromField, ToField, FromJSON, ToJSON, NFData)
    via UUID
  deriving
    (Display)
    via ShowInstance UUID

-- | a wrapper that attaches from and tofield instances
--  for a text db row for LucidHtml
newtype TextHtml = MkTextHtml (Lucid.Html ())
  deriving stock (Show, Generic)

instance ToJSON TextHtml where
  toJSON (MkTextHtml a) = String $ Text.toStrict $ Lucid.renderText a

instance FromJSON TextHtml where
  parseJSON = withText "TextHtml" (pure . MkTextHtml . Lucid.toHtmlRaw @Text)

instance NFData TextHtml where
  rnf a = seq a ()

instance Eq TextHtml where
  (==) (MkTextHtml a) (MkTextHtml b) = Lucid.renderText a == Lucid.renderText b

--
instance FromField TextHtml where
  fromField field bs = MkTextHtml . Lucid.toHtmlRaw @Text <$> fromField field bs

instance ToField TextHtml where
  toField (MkTextHtml x) = toField $ Lucid.renderText x

data Release = Release
  { releaseId :: ReleaseId
  , packageId :: PackageId
  , version :: Version
  , archiveChecksum :: Text
  , uploadedAt :: Maybe UTCTime
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , readme :: Maybe TextHtml
  , readmeStatus :: ImportStatus
  , changelog :: Maybe TextHtml
  , changelogStatus :: ImportStatus
  , license :: SPDX.License
  , sourceRepos :: Vector Text
  , homepage :: Maybe Text
  , documentation :: Text
  , bugTracker :: Maybe Text
  , maintainer :: Text
  , synopsis :: Text
  , description :: Text
  , flags :: ReleaseFlags
  , testedWith :: Vector Version
  , deprecated :: Maybe Bool
  , repository :: Maybe Text
  , revisedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[TableName "releases"] Release)

instance Ord Release where
  compare x y = compare x.version y.version

newtype ReleaseFlags = ReleaseFlags (Vector PackageFlag)
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, NFData, ToSchema)
  deriving (ToField, FromField) via Aeson ReleaseFlags

data ImportStatus
  = Imported
  | Inexistent
  | NotImported
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)
  deriving
    (ToJSON, FromJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] ImportStatus)

parseImportStatus :: ByteString -> Maybe ImportStatus
parseImportStatus "imported" = pure Imported
parseImportStatus "inexistent" = pure Inexistent
parseImportStatus "not-imported" = pure NotImported
parseImportStatus _ = Nothing

instance Display ImportStatus where
  displayBuilder Imported = "imported"
  displayBuilder Inexistent = "inexistent"
  displayBuilder NotImported = "not-imported"

instance FromField ImportStatus where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField _ (Just bs) | Just status <- parseImportStatus bs = pure status
  fromField f (Just bs) =
    returnError ConversionFailed f $
      unpack $
        "Conversion error: Expected component to be one of "
          <> display @[ImportStatus] [minBound .. maxBound]
          <> ", but instead got "
          <> decodeUtf8 bs

instance ToField ImportStatus where
  toField = Escape . encodeUtf8 . display

newtype SupportedCompilers = Vector (CompilerFlavor, VersionRange)
  deriving stock (Eq, Show, Generic, Typeable)
  deriving
    (ToJSON, FromJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] SupportedCompilers)

data ReleaseDeprecation = ReleaseDeprecation
  { deprecated :: Bool
  , release :: ReleaseId
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving
    (ToJSON, FromJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] ReleaseDeprecation)
  deriving (ToField, FromField) via Aeson ReleaseDeprecation

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Release)
