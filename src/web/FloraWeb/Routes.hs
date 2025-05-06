{-# LANGUAGE CPP #-}

module FloraWeb.Routes where

import Data.OpenApi (OpenApi)
import Data.Text (Text)
import Servant.API
import Servant.API.Generic
import Text.XML

import FloraWeb.API.Routes qualified as API
import FloraWeb.Common.OpenSearch
import FloraWeb.Feed.Routes qualified as Feed
import FloraWeb.Pages.Routes qualified as Pages

type ServerRoutes = NamedRoutes Routes

data Routes mode = Routes
  { assets :: mode :- "static" :> Raw
  , feed :: mode :- "feed" :> Feed.Routes
  , openSearch :: mode :- "opensearch.xml" :> Get '[OpenSearchXML] Document
  , pages :: mode :- AuthProtect "optional-cookie-auth" :> Pages.Routes
  , api :: mode :- API.Routes
  , openApi
      :: mode
        :- "documentation"
          :> "openapi.json"
          :> Get '[JSON] OpenApi
  , docs :: mode :- "documentation" :> Raw
  , livereload :: mode :- "livereload" :> Get '[PlainText] (Headers '[Header "HX-Refresh" Text] NoContent)
  , favicon :: mode :- Raw
  }
  deriving stock (Generic)
