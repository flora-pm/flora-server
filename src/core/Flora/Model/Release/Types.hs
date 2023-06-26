module Flora.Model.Release.Types
  ( ReleaseId (..)
  , TextHtml (..)
  , Release (..)
  , ReleaseMetadata (..)
  , ImportStatus (..)
  , SupportedCompilers (..)
  , ReleaseDeprecation (..)
  )
where

import Data.Aeson
import Data.Aeson.Orphans ()
import Data.ByteString (ByteString)
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
import Distribution.Orphans ()
import Distribution.SPDX.License ()
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.Flag (PackageFlag)
import Distribution.Types.Version
import Distribution.Types.VersionRange (VersionRange)
import GHC.Generics (Generic)
import Lucid qualified

import Control.DeepSeq
import Data.Text.Lazy qualified as Text
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
  toJSON (MkTextHtml a) = String $! Text.toStrict $! Lucid.renderText a

instance FromJSON TextHtml where
  parseJSON = withText "TextHtml" (\text -> pure $! MkTextHtml $! Lucid.toHtmlRaw @Text text)

instance NFData TextHtml where
  rnf a = seq a ()

instance Eq TextHtml where
  (==) (MkTextHtml a) (MkTextHtml b) = Lucid.renderText a == Lucid.renderText b

--
instance FromField TextHtml where
  fromField field bs = MkTextHtml . Lucid.toHtmlRaw @Text <$> fromField field bs

instance ToField TextHtml where
  toField (MkTextHtml x) = toField $! Lucid.renderText x

data Release = Release
  { releaseId :: ReleaseId
  -- ^ The unique ID of this release
  , packageId :: PackageId
  -- ^ The package to which this release is linked
  , version :: Version
  -- ^ The version that this release represents
  , metadata :: ReleaseMetadata
  -- ^ Metadata associated with this release
  , archiveChecksum :: Text
  -- ^ The SHA256 checksum of the stored archive for this release
  , uploadedAt :: Maybe UTCTime
  , --  ^ The timestamp of upload, provided by Hackage
    createdAt :: UTCTime
  -- ^ Date of creation of this release
  , updatedAt :: UTCTime
  -- ^ Last update timestamp for this release
  , readme :: Maybe TextHtml
  -- ^ Content of the release's README
  , readmeStatus :: ImportStatus
  -- ^ Import status of the README
  , changelog :: Maybe TextHtml
  -- ^ Content of the release's Changelog
  , changelogStatus :: ImportStatus
  -- ^ Repo - where this package has been imported from
  , repository :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData, FromJSON, ToJSON)
  deriving
    (Entity)
    via (GenericEntity '[TableName "releases"] Release)

instance Ord Release where
  compare x y = compare (x.version) (y.version)

data ImportStatus
  = Imported
  | Inexistent
  | NotImported
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

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
  deriving anyclass (ToJSON, FromJSON, NFData)

data ReleaseMetadata = ReleaseMetadata
  { license :: SPDX.License
  , sourceRepos :: Vector Text
  , homepage :: Maybe Text
  , documentation :: Text
  , bugTracker :: Maybe Text
  , maintainer :: Text
  , synopsis :: Text
  , description :: Text
  , flags :: Vector PackageFlag
  , testedWith :: Vector Version
  , deprecated :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, NFData)
  deriving (ToField, FromField) via Aeson ReleaseMetadata

data ReleaseDeprecation = ReleaseDeprecation
  { deprecated :: Bool
  , release :: ReleaseId
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)
  deriving (ToField, FromField) via Aeson ReleaseDeprecation
