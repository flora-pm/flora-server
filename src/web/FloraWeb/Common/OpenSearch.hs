-- Copyright (c) 2013 Alexander Bondarenko
--
-- Permission is hereby granted, free of charge, to any person obtaining a
-- copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
{-# OPTIONS_GHC -Wno-orphans #-}

module FloraWeb.Common.OpenSearch
  ( OpenSearchXML
  , openSearchHandler
  ) where

import Control.Monad.Writer.Strict
import Data.DList qualified as DL
import Data.Default ()
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Media qualified as M
import Servant.API
import Text.XML
import Text.XML qualified as XML

data OpenSearchXML

instance Accept OpenSearchXML where
  contentType _ = "application" M.// "opensearchdescription+xml" M./: ("charset", "utf-8")

instance MimeRender OpenSearchXML XML.Document where
  mimeRender _ = XML.renderLBS def

openSearchHandler :: Monad m => m XML.Document
openSearchHandler =
  pure $
    openSearchDocument "OpenSearchDescription" $ do
      element "ShortName" $ content "Flora"
      element "Description" $ content "Search on Flora"
      element "InputEncoding" $ content "UTF-8"
      elementA "Url" [("type", "text/html"), ("method", "get"), ("template", "https://flora.pm/search?q={searchTerms}&ref=opensearch")] empty
      element "moz:searchForm" $ content "https://flora.pm"

openSearchDocument :: XML.Name -> XML -> XML.Document
openSearchDocument name children =
  XML.Document
    { documentPrologue = XML.Prologue def def def
    , documentRoot = XML.Element name (Map.fromList [("xmlns", "http://a9.com/-/spec/opensearch/1.1/"), ("xmlns:moz", "http://www.mozilla.org/2006/browser/search/")]) (render children)
    , documentEpilogue = def
    }

-- | Node container to be rendered as children nodes.
type XML = Writer (DL.DList Node) ()

-- | Convert collected nodes to a list of child nodes.
render :: XML -> [Node]
render = DL.toList . execWriter

-- | Do nothing.
empty :: XML
empty = return ()

-- | Insert one node.
node :: Node -> XML
node = tell . DL.singleton

-- | Insert an "Element" node constructed with name and children.
element :: ToXML a => Name -> a -> XML
element name children = node . NodeElement $! Element name def (render $ toXML children)

-- | Insert an "Element" node constructed with name, attributes and children.
elementA :: ToXML a => Name -> [(Name, Text)] -> a -> XML
elementA name attrs children = node . NodeElement $! Element name (M.fromList attrs) (render $ toXML children)

-- | Insert text content node.
content :: Text -> XML
content = node . NodeContent

-- | Provide instances for this class to use your data
-- as "XML" nodes.
class ToXML a where
  toXML :: a -> XML

-- | Do nothing.
instance ToXML () where
  toXML () = empty

-- | Insert already prepared nodes.
instance ToXML XML where
  toXML = id

-- | Don't use [Char] please, it will scare OverloadedStrings.
instance ToXML Text where
  toXML = content

-- | XML schema uses lower case.
instance ToXML Bool where
  toXML True = "true"
  toXML False = "false"

instance ToXML Float where
  toXML = content . T.pack . show

instance ToXML Double where
  toXML = content . T.pack . show

instance ToXML Int where
  toXML = content . T.pack . show

instance ToXML Integer where
  toXML = content . T.pack . show

instance ToXML Char where
  toXML = content . T.singleton

-- | Insert node if available. Otherwise do nothing.
instance ToXML a => ToXML (Maybe a) where
  toXML = maybe empty toXML

instance IsString XML where
  fromString = content . T.pack
