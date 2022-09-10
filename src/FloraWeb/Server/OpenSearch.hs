module FloraWeb.Server.OpenSearch where

import Network.HTTP.Media qualified as M
import Text.XML qualified as XML
import Text.XML.Writer as XML
import Servant.API
import Data.Map qualified as Map
import Data.Default (def)

data OpenSearchXML

instance Accept OpenSearchXML where
  contentType _ = "application" M.// "opensearchdescription+xml" M./: ("charset", "utf-8")

instance MimeRender OpenSearchXML XML.Document where
  mimeRender _ = XML.renderLBS def

openSearchHandler :: (Monad m) => m XML.Document
openSearchHandler = pure $
  openSearchDocument "OpenSearchDescription" $ do
    element "ShortName" $ content "Flora"
    element "Description" $ content "Search on Flora"
    element "InputEncoding" $ content "UTF-8"
    elementA "Url" [("type", "text/html"), ("method", "get"), ("template", "https://flora.pm/search?q={searchTerms}&ref=opensearch")] empty
    element "moz:searchForm" $ content "https://flora.pm"

openSearchDocument :: XML.Name -> XML.XML -> XML.Document
openSearchDocument name children =
  XML.Document { documentPrologue = XML.Prologue def def def
           , documentRoot = XML.Element name (Map.fromList [("xmlns", "http://a9.com/-/spec/opensearch/1.1/"), ("xmlns:moz", "http://www.mozilla.org/2006/browser/search/")]) (render children)
           , documentEpilogue = def
           }
