module FloraWeb.Atom where

import Data.Data
import Data.Function
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Time
import Data.Time qualified as Time
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Network.HTTP.Media qualified as M
import Servant.API
import Text.Atom.Feed (Entry (..), Feed (..))
import Text.Atom.Feed qualified as Atom
import Text.Feed.Export qualified as Export
import Text.Feed.Types

import Flora.Model.Feed.Types

data Atom deriving stock (Typeable)

instance Accept Atom where
  contentTypes _ =
    NE.singleton $
      "application" M.// "atom+xml" M./: ("charset", "utf-8")

instance MimeRender Atom Atom.Feed where
  mimeRender _ = TLE.encodeUtf8 . fromJust . Export.textFeed . AtomFeed

makeFeed
  :: Text
  -- ^ Hostname of the instance
  -> UTCTime
  -- ^ Timestamp of last update
  -> Vector FeedEntry
  -> Atom.Feed
makeFeed feedId updated entries =
  let feedTitle = Atom.TextString $ "Flora Feed"
      feedUpdated = Text.pack $ Time.formatTime defaultTimeLocale "%a, %_d %b %Y" updated
      feedEntries =
        entries
          & fmap makeAtomEntry
          & Vector.toList
      feed = Atom.nullFeed feedId feedTitle feedUpdated
   in feed{feedEntries}

makeAtomEntry :: FeedEntry -> Entry
makeAtomEntry feedEntry =
  let entryId = UUID.toText feedEntry.entryId
      entryTitle = Atom.TextString feedEntry.title
      entryUpdated = Text.pack $ Time.formatTime defaultTimeLocale "%a, %_d %b %Y" feedEntry.updatedAt
   in Atom.nullEntry entryId entryTitle entryUpdated
