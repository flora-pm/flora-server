module FloraWeb.Atom where

import Data.Data
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Text.Lazy.Encoding qualified as TLE
import Network.HTTP.Media qualified as M
import Servant.API
import Text.Atom.Feed qualified as Atom
import Text.Feed.Export qualified as Export
import Text.Feed.Types (Feed (AtomFeed))

data Atom deriving stock (Typeable)

instance Accept Atom where
  contentTypes _ =
    NE.singleton $
      "application" M.// "atom+xml" M./: ("charset", "utf-8")

instance MimeRender Atom Atom.Feed where
  mimeRender _ = TLE.encodeUtf8 . fromJust . Export.textFeed . AtomFeed
