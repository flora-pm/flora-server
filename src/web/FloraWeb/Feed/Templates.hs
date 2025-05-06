{-# LANGUAGE QuasiQuotes #-}

module FloraWeb.Feed.Templates
  ( showFeedsBuilderPage
  , showSearchedPackages
  ) where

import Control.Monad.Reader.Class qualified as Reader
import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Htmx.Lucid.Core
import Lucid
import PyF

import Flora.Environment.Env
import Flora.Model.Package
import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Components.Utils
import FloraWeb.Pages.Templates

showFeedsBuilderPage :: FloraHTML
showFeedsBuilderPage = do
  env <- Reader.ask
  let baseURL =
        case env.environment of
          Production -> "https://" <> env.domain
          _ -> "http://" <> env.domain <> ":" <> (display env.httpPort)

  banner
  let alpineData =
        [fmt| {{
          activeFilters: new Array(),
          urlBase: "{baseURL}/feed/atom.xml?packages[]=",
          get url() {{ return this.activeFilters.length === 0 ? "" : this.urlBase + this.activeFilters.join('&packages[]=') }}
        }} |]
  div_ [class_ "container container--small", xData_ alpineData] $ do
    div_ [class_ "feed-package-selector"] $ do
      packageSelector
    div_ [class_ "searched-packages"] $ mempty
    section_ [class_ "selected-packages"] $ do
      div_
        [ class_ "generated-feed-url"
        , xHtml_ "'<a href=\"' + url + '\">' + url + '</a>'"
        ]
        mempty
      template_ [xFor_ "(package, index) in activeFilters", key_ "package"]
        $ button_
          [ name_ "package"
          , class_ "selected_package"
          , xBind_ "id" "index"
          , type_ "button"
          , xOn_ "click" "activeFilters.splice(index, 1)"
          ]
        $ do
          span_ [xText_ "package"] $ mempty
          Icons.cross

banner :: FloraHTML
banner = do
  div_ [class_ "relative"] $
    h1_ [class_ "main-title"] $
      span_ [class_ "main-title"] "Search packages to follow"

packageSelector :: FloraHTML
packageSelector =
  input_
    [ class_ "feed-package-search"
    , type_ "search"
    , placeholder_ "Begin typing…"
    , hxPost_ "/feed/search"
    , hxTrigger_ "input changed delay:100ms, keyup[key=='Enter'], load"
    , hxSwap_ "innerHTML"
    , name_ "search"
    , hxTarget_ ".searched-packages"
    , autocomplete_ "off"
    ]

showSearchedPackages :: Vector (Namespace, PackageName) -> FloraHTML
showSearchedPackages packages = do
  Vector.forM_ packages $ \(namespace@(Namespace nsText), packageName) -> do
    let qualifiedName = display namespace <> "/" <> display packageName
    let idName = nsText <> "-" <> display packageName
    div_ [] $ do
      input_
        [ id_ ("selected-" <> idName)
        , name_ ("selected-" <> idName)
        , type_ "checkbox"
        , class_ "searched-package"
        , value_ qualifiedName
        , xModel_ [] "activeFilters"
        ]
      label_ [for_ ("selected-" <> idName)] $ toHtml qualifiedName
