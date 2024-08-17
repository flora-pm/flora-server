{-# LANGUAGE CPP #-}

module FloraWeb.Routes where

import Data.OpenApi (OpenApi)
import Servant
import Servant.API.Generic
import Text.XML

import FloraWeb.API.Routes qualified as API
import FloraWeb.Common.OpenSearch
import FloraWeb.Pages.Routes qualified as Pages
import Servant.API.WebSocket (WebSocket)

type ServerRoutes = NamedRoutes Routes

data Routes mode = Routes
  { assets :: mode :- "static" :> Raw
  , openSearch :: mode :- "opensearch.xml" :> Get '[OpenSearchXML] Document
  , pages :: mode :- AuthProtect "optional-cookie-auth" :> Pages.Routes
  , api :: mode :- "api" :> API.Routes
  , openApi
      :: mode
        :- "documentation"
          :> "openapi.json"
          :> Get '[JSON] OpenApi
  , docs :: mode :- "documentation" :> Raw
  , livereload :: mode :- "livereload" :> WebSocket
  }
  deriving stock (Generic)
