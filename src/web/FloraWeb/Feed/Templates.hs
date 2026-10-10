{-# LANGUAGE QuasiQuotes #-}

module FloraWeb.Feed.Templates
  ( showFeedsBuilderPage
  , showSearchedPackages
  )
where

import Control.Monad.Reader
import Control.Monad.Reader.Class qualified as Reader
import Data.Text (Text)
import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Htmx.Lucid.Core
import Lucid
import PyF

import Flora.Environment.Config
import Flora.Model.Package.Types
import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Components.Utils
import FloraWeb.Pages.Templates.Types
  ( FloraHTML
  , TemplateEnv (TemplateEnv, assets, domain, environment, httpPort)
  )

jsHtmxLink :: FloraHTML
jsHtmxLink = do
  TemplateEnv{assets, environment} <- ask
  let jsHtmxURL = "/static/" <> assets.jsHtmx.name
  case environment of
    Production ->
      script_ [src_ jsHtmxURL, type_ "module", defer_ "", integrity_ ("sha256-" <> assets.jsHtmx.hash)] ("" :: Text)
    _ ->
      script_ [src_ jsHtmxURL, type_ "module", defer_ ""] ("" :: Text)

jsAlpineLink :: FloraHTML
jsAlpineLink = do
  TemplateEnv{assets, environment} <- ask
  let jsAlpineURL = "/static/" <> assets.jsAlpine.name
  case environment of
    Production ->
      script_ [src_ jsAlpineURL, type_ "module", defer_ "", integrity_ ("sha256-" <> assets.jsAlpine.hash)] ("" :: Text)
    _ ->
      script_ [src_ jsAlpineURL, type_ "module", defer_ ""] ("" :: Text)

showFeedsBuilderPage :: FloraHTML
showFeedsBuilderPage = do
  templateEnv <- Reader.ask
  let baseURL =
        case templateEnv.environment of
          Production -> "https://" <> templateEnv.domain
          _ -> "http://" <> templateEnv.domain <> ":" <> display templateEnv.httpPort

  banner
  let alpineData =
        [fmt| {{
          activeFilters: new Array(),
          urlBase: "{baseURL}/feed/atom.xml?packages[]=",
          get url() {{ return this.activeFilters.length === 0 ? "" : this.urlBase + this.activeFilters.join('&packages[]=') }}
        }} |]
  div_ [class_ "wrapper inset-large", id_ "content", xData_ alpineData] $ do
    div_ [class_ "package-about aside aside--start"] $ do
      packageSelector
      div_ [class_ "flow flow--small"] $ do
        h3_ [class_ "title-section"] "Results"
        div_ [class_ "searched-packages"] mempty
        section_ [class_ "selected-packages"] $ do
          div_
            [ class_ "generated-feed-url"
            , xHtml_ "'<a href=\"' + url + '\">' + url + '</a>'"
            ]
            mempty
          template_ [xFor_ "(package, index) in activeFilters", key_ "package"]
            $ button_
              [ name_ "package"
              , class_ "selected_package flex items-center gap gap--tiny"
              , xBind_ "id" "index"
              , type_ "button"
              , xOn_ "click" "activeFilters.splice(index, 1)"
              ]
            $ do
              span_ [xText_ "package"] mempty
              Icons.cross
  jsHtmxLink
  jsAlpineLink

banner :: FloraHTML
banner = do
  header_ [class_ "pageHead"] $ do
    div_ [class_ "wrapper"] $ do
      div_ [class_ "flow"] $ do
        h1_ [class_ "pageHead-title"] "Packages feed"
        p_ [class_ "pageHead-subtitle"] "Generate an Atom feed to follow updates"

packageSelector :: FloraHTML
packageSelector =
  aside_ [class_ "flex flex-col flex-no-grow"] $ do
    div_ [class_ "flex flex-col flow flow--small"] $ do
      h3_ [id_ "feed-package-selector-title", class_ "title-section"] "Search packages"
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
        , ariaLabelledby_ "feed-package-selector-title"
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
