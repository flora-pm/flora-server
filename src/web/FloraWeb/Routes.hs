{-# LANGUAGE CPP #-}

module FloraWeb.Routes where

import Data.OpenApi (OpenApi)
import Servant.API hiding (ServerSentEvents)
import Servant.API.EventStream (ServerSentEvents)
import Servant.API.Generic
import Text.XML

import FloraWeb.API.Routes qualified as API
import FloraWeb.Common.OpenSearch
import FloraWeb.Feed.Routes qualified as Feed
import FloraWeb.LiveReload (ReloadEvent)
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
  , livereload :: mode :- "livereload" :> ServerSentEvents (SourceIO ReloadEvent)
  }
  deriving stock (Generic)
