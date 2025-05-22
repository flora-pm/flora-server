module FloraWeb.Atom where

import Data.Data
import Data.Function
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust, maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Time
import Data.Time.Format.ISO8601 qualified as Time
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word16)
import Network.HTTP.Media qualified as M
import Servant.API
import Text.Atom.Feed (Entry (..), Feed (..), Link (..))
import Text.Atom.Feed qualified as Atom
import Text.Feed.Export qualified as Export
import Text.Feed.Types
import Text.XML

import Flora.Model.Feed.Types

data Atom deriving stock (Typeable)

instance Accept Atom where
  contentTypes _ =
    NE.singleton $
      "application" M.// "xml" M./: ("charset", "utf-8")

instance MimeRender Atom Atom.Feed where
  mimeRender _ atom =
    let feed = TLE.encodeUtf8 . fromJust . Export.textFeedWith (def{rsXMLDeclaration = False}) $ AtomFeed atom
     in "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          <> "<?xml-stylesheet href=\"/static/feed/pretty-atom-feed.xsl\" type=\"text/xsl\"?>\n"
          <> feed

makeFeed
  :: Either (Text, Word16) Text
  -- ^ Hostname of the instance
  -> UTCTime
  -- ^ Timestamp of last update
  -> Vector FeedEntry
  -> Atom.Feed
makeFeed instanceInfo updated entries =
  let feedTitle = Atom.TextString "Flora Feed"
      feedUpdated = Text.pack $ Time.formatShow Time.iso8601Format updated
      feedId =
        case instanceInfo of
          Right hostname -> "http://" <> hostname
          Left (hostname, port) -> "http://" <> hostname <> ":" <> display port
      floraLink = Atom.nullLink feedId
      feedLinks =
        [ floraLink
        , floraLink{linkRel = Just (Right "self")}
        ]
      feedEntries =
        entries
          & fmap makeAtomEntry
          & Vector.toList
      feed = Atom.nullFeed feedId feedTitle feedUpdated
   in feed{feedEntries, feedLinks}

makeAtomEntry :: FeedEntry -> Entry
makeAtomEntry FeedEntry{entryId = feedEntryId, title, updatedAt, link, content} =
  let entryId = UUID.toText feedEntryId
      entryTitle = Atom.TextString title
      entryUpdated = Text.pack $ Time.formatShow Time.iso8601Format updatedAt
      entryContent = Just $ Atom.TextContent content
      entry = Atom.nullEntry entryId entryTitle entryUpdated
      entryLinks = maybeToList (Atom.nullLink <$> link)
   in entry{entryContent, entryLinks}
