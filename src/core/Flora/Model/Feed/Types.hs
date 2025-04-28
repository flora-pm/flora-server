module Flora.Model.Feed.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Text (Text)
import Data.Text.Display
import Data.Time
import Data.UUID (UUID)
import Data.Word
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Deriving.Aeson qualified as Aeson
import Distribution.Types.Version
import Effectful
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.Generics
import Heptapod
import Web.HttpApiData (toUrlPiece)

import Flora.Model.Package.Types

data FeedEntry = FeedEntry
  { entryId :: UUID
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

newReleaseEntry
  :: (IOE :> es, Time :> es)
  => Either (Text, Word16) Text
  -> Package
  -> Version
  -> Eff es FeedEntry
newReleaseEntry instanceInfo package version = do
  let floraLink =
        case instanceInfo of
          Right hostname -> "http://" <> hostname
          Left (hostname, port) -> "http://" <> hostname <> ":" <> display port
  entryId <- liftIO Heptapod.generate
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
