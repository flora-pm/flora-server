module Flora.Model.Release.Types
  ( ReleaseId (..)
  , TextHtml (..)
  , Release (..)
  , ReleaseMetadata (..)
  , ReadmeStatus (..)
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
import Database.PostgreSQL.Entity.Types (Entity, GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (..), ResultError (..), returnError)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))
import Distribution.SPDX.License ()
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.Version
import GHC.Generics (Generic)

import Distribution.Orphans ()
import Flora.Model.Package
import Lucid qualified

newtype ReleaseId = ReleaseId {getReleaseId :: UUID}
  deriving
    (Eq, Ord, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID
  deriving
    (Display)
    via ShowInstance UUID

{-| a wrapper that attaches from and tofield instances
 for a text db row for LucidHtml
-}
newtype TextHtml = MkTextHtml (Lucid.Html ())
  deriving stock (Show, Generic)

instance Eq TextHtml where
  (==) (MkTextHtml a) (MkTextHtml b) = Lucid.renderText a == Lucid.renderText b

--
instance FromField TextHtml where
  fromField field bs = MkTextHtml . Lucid.toHtmlRaw @Text <$> fromField field bs
instance ToField TextHtml where
  toField (MkTextHtml x) = toField $ Lucid.renderText x

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
  , readmeStatus :: ReadmeStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "releases"] Release)

instance Ord Release where
  compare x y = compare (x.version) (y.version)

data ReadmeStatus
  = Imported
  | Inexistent
  | NotImported
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

parseReadmeStatus :: ByteString -> Maybe ReadmeStatus
parseReadmeStatus "imported" = pure Imported
parseReadmeStatus "inexistent" = pure Inexistent
parseReadmeStatus "not-imported" = pure NotImported
parseReadmeStatus _ = Nothing

instance Display ReadmeStatus where
  displayBuilder Imported = "imported"
  displayBuilder Inexistent = "inexistent"
  displayBuilder NotImported = "not-imported"

instance FromField ReadmeStatus where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField _ (Just bs) | Just status <- parseReadmeStatus bs = pure status
  fromField f (Just bs) = returnError ConversionFailed f $ unpack $ "Conversion error: Expected component to be one of " <> display @[ReadmeStatus] [minBound .. maxBound] <> ", but instead got " <> decodeUtf8 bs

instance ToField ReadmeStatus where
  toField = Escape . encodeUtf8 . display

data ReleaseMetadata = ReleaseMetadata
  { license :: SPDX.License
  , sourceRepos :: [Text]
  , homepage :: Maybe Text
  , documentation :: Text
  , bugTracker :: Maybe Text
  , maintainer :: Text
  , synopsis :: Text
  , description :: Text
  }
  deriving stock (Eq, Ord, Show, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON)
  deriving (ToField, FromField) via Aeson ReleaseMetadata
