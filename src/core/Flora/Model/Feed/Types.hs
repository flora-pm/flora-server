module Flora.Model.Feed.Types
  ( FeedEntry (..)
  , FeedEntryId (..)
  , deterministicFeedEntryId
  , newReleaseEntry
  ) where

import Control.DeepSeq
import Crypto.Hash.MD5 qualified as MD5
import Data.Aeson
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Display
import Data.Time
import Data.UUID (UUID, fromByteString, toByteString)
import Data.Word
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson qualified as Aeson
import Distribution.Types.Version
import Distribution.Utils.Structured (structuredEncode)
import Effectful
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.Generics
import Web.HttpApiData (toUrlPiece)

import Flora.Model.Package.Types

newtype FeedEntryId = FeedEntryId UUID
  deriving
    (Display)
    via ShowInstance UUID
  deriving
    (Eq, FromField, FromJSON, NFData, Ord, Show, ToField, ToJSON)
    via UUID

data FeedEntry = FeedEntry
  { entryId :: FeedEntryId
  , title :: Text
  , link :: Maybe Text
  , content :: Text
  , packageId :: PackageId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (FromJSON, ToJSON)
    via (Aeson.CustomJSON '[Aeson.FieldLabelModifier '[Aeson.CamelToSnake]] FeedEntry)
  deriving
    (Entity)
    via (GenericEntity '[TableName "package_feeds"] FeedEntry)

-- | Generates a feed entry id deterministically by hashing the package id and version it announces.
--
-- Concurrent importers can both take the \"this release does not exist yet\" branch of
-- 'Flora.Model.Release.Update.upsertRelease' for the same release, so a random id would let both of
-- them announce it. Deriving the id from what is announced is what makes the @ON CONFLICT DO
-- NOTHING@ of 'Flora.Model.Feed.Update.insertFeedEntry' able to fire at all.
deterministicFeedEntryId :: PackageId -> Version -> FeedEntryId
deterministicFeedEntryId (PackageId packageId) version =
  FeedEntryId . fromJust . fromByteString . fromStrict . MD5.hash . toStrict $
    toByteString packageId <> structuredEncode version <> "new-release"

newReleaseEntry
  :: Time :> es
  => Either (Text, Word16) Text
  -> Package
  -> Version
  -> Eff es FeedEntry
newReleaseEntry instanceInfo package version = do
  let floraLink =
        case instanceInfo of
          Right hostname -> "http://" <> hostname
          Left (hostname, port) -> "http://" <> hostname <> ":" <> display port
  let entryId = deterministicFeedEntryId package.packageId version
  let title = display package.namespace <> "/" <> display package.name <> " v" <> display version
  let link = toUrlPiece $ floraLink <> "/packages/" <> display package.namespace <> "/" <> display package.name <> "/" <> display version
  let content = display package.namespace <> "/" <> display package.name <> " v" <> display version <> " has been released. See its changelog at " <> toUrlPiece (floraLink <> "/packages/" <> display package.namespace <> "/" <> display package.name <> "/" <> display version <> "/changelog")
  now <- Time.currentTime
  pure $
    FeedEntry
      entryId
      title
      (Just link)
      content
      package.packageId
      now
      now
