module Flora.Model.Feed.Types where

import Control.DeepSeq
import Data.Text (Text)
import Data.Text.Display
import Data.Time
import Data.UUID (UUID)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Deriving.Aeson
import Distribution.Types.Version
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Heptapod
import Web.HttpApiData (toUrlPiece)

import Flora.Environment.Env
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
    (Entity)
    via (GenericEntity '[TableName "feed_entries"] FeedEntry)

newReleaseEntry
  :: (IOE :> es, Reader FloraEnv :> es, Time :> es)
  => Package
  -> Version
  -> Eff es FeedEntry
newReleaseEntry package version = do
  env <- Reader.ask
  let port = if env.environment == Production then "" else ":" <> display env.httpPort
  let linkBase = "http://" <> env.domain <> port
  entryId <- liftIO Heptapod.generate
  let title = display package.namespace <> "/" <> display package.name <> " v" <> display version
  let link = toUrlPiece $ linkBase <> "/packages/" <> display package.namespace <> "/" <> display package.name <> "/" <> display version
  let content = display package.namespace <> "/" <> display package.name <> " v" <> display version <> " has been released. See its changelog at " <> toUrlPiece (linkBase <> "/packages/" <> display package.namespace <> "/" <> display package.name <> "/" <> display version <> "/changelog")
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
