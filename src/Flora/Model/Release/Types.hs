module Flora.Model.Release.Types
  ( ReleaseId (..)
  , TextHtml (..)
  , Release (..)
  , ReleaseMetadata (..)
  , ImportStatus (..)
  , Repo (..)
  , RepoService (..)
  , fromSourceRepo
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
import Distribution.Types.SourceRepo (RepoType (..), SourceRepo (..))
import Distribution.Types.Version
import GHC.Generics (Generic)
import Network.URL qualified as URL

import Data.Text qualified as Text
import Data.Vector (Vector)
import Distribution.Orphans ()
import Distribution.Types.Flag (PackageFlag)
import Flora.Model.Package
import Lucid qualified
import Network.URL (URLType (..))

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
  , readmeStatus :: ImportStatus
  -- ^ Import status of the README
  , changelog :: Maybe TextHtml
  -- ^ Content of the release's Changelog
  , changelogStatus :: ImportStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
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

data RepoService
  = GitHub
  | GitLab
  | Bitbucket
  | DarcsDen
  | SourceHut
  | PijulNest
  deriving stock (Eq, Ord, Show, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON)
  deriving (ToField, FromField) via Aeson RepoService

data Repo = Repo
  { url :: Maybe Text
  , provider :: Maybe RepoService
  , repoType :: Maybe RepoType
  }
  deriving stock (Eq, Ord, Show, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON)
  deriving (ToField, FromField) via Aeson Repo

fromSourceRepo :: SourceRepo -> Repo
fromSourceRepo sourceRepo = Repo{..}
  where
    url = fmap display sourceRepo.repoLocation
    repoType = sourceRepo.repoType
    provider = url >>= inferProvider

inferProvider :: Text -> Maybe RepoService
inferProvider urlString =
  URL.importURL (Text.unpack urlString)
    >>= \url -> case url.url_type of
      Absolute hostStruct ->
        case hostStruct.host of
          "www.github.com" -> Just GitHub
          "github.com" -> Just GitHub
          "www.gitlab.com" -> Just GitLab
          "gitlab.com" -> Just GitLab
          "bitbucket.org" -> Just Bitbucket
          "hub.darcs.net" -> Just DarcsDen
          "sr.ht" -> Just SourceHut
          "nest.pijul.com" -> Just PijulNest
          _ -> Nothing
      _ -> Nothing

data ReleaseMetadata = ReleaseMetadata
  { license :: SPDX.License
  , repo :: Maybe Repo
  , homepage :: Maybe Text
  , documentation :: Text
  , bugTracker :: Maybe Text
  , maintainer :: Text
  , synopsis :: Text
  , description :: Text
  , flags :: Vector PackageFlag
  }
  deriving stock (Eq, Ord, Show, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON)
  deriving (ToField, FromField) via Aeson ReleaseMetadata
