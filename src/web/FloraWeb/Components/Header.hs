{-# LANGUAGE QuasiQuotes #-}

module FloraWeb.Components.Header where

import Control.Monad (unless)
import Control.Monad.Reader
import Data.Text (Text)
import Lucid
import PyF

import Flora.Environment.Config
import FloraWeb.Components.Navbar (navbar)
import FloraWeb.Components.SkipLink (skipLink)
import FloraWeb.Components.Utils
import FloraWeb.Links (renderAbsoluteLink)
import FloraWeb.Pages.Templates.Types (FloraHTML, TemplateEnv (..))

header :: FloraHTML
header = do
  TemplateEnv{environment, title, theme, seoIndexing} <- ask
  doctype_
  let theme' = case theme of
        Nothing -> []
        Just a -> [data_ "theme" a]
  html_
    ( [ lang_ "en"
      , class_ "no-js"
      ]
        <> theme'
    )
    $ do
      head_ $ do
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        unless seoIndexing $ meta_ [name_ "robots", content_ "noindex"]
        link_ [rel_ "apple-touch-icon", sizes_ "180x180", href_ "/static/icons/apple-touch-icon.png"]
        case environment of
          Development -> do
            link_ [rel_ "icon", href_ "/static/icons/favicon-dev.svg", type_ "image/svg+xml"]
            link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "/static/icons/favicon-dev-32x32.png"]
            link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "/static/icons/favicon-dev-16x16.png"]
          _ -> do
            link_ [rel_ "icon", href_ "/static/icons/favicon.svg", type_ "image/svg+xml"]
            link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "/static/icons/favicon-32x32.png"]
            link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "/static/icons/favicon-16x16.png"]
        link_ [rel_ "manifest", href_ "/static/icons/site.webmanifest"]
        meta_ [name_ "color-scheme", content_ "light dark"]
        meta_ [name_ "theme-color", content_ "#654480"]
        meta_ [name_ "theme-color", content_ "#c399e8", media_ "(prefers-color-scheme: dark)"]

        title_ (text title)

        script_ [type_ "module"] $ do
          toHtmlRaw @Text
            [str|
          document.documentElement.classList.remove('no-js');
          document.documentElement.classList.add('js');
          |]

        jsPolyfillsLink
        cssLink
        link_
          [ rel_ "search"
          , type_ "application/opensearchdescription+xml"
          , title_ "Flora"
          , href_ "/opensearch.xml"
          ]
        meta_ [name_ "description", content_ "A package repository for the Haskell ecosystem"]
        ogTags
        meta_ [name_ "fediverse:creator", content_ "@flora_pm@functional.cafe"]
      -- link_ [rel_ "canonical", href_ $ getCanonicalURL assigns]

      body_ [] $ do
        case environment of
          Development ->
            script_ [type_ "module"] $
              toHtmlRaw @Text
                [str|
          const floraLiveReload = new EventSource("/livereload");
          let floraReloadErrored = false;
          floraLiveReload.onopen = () => floraReloadErrored && window.location.reload();
          floraLiveReload.addEventListener("reload", () => window.location.reload());
          floraLiveReload.onerror = () => floraReloadErrored = true;
          |]
          _ -> mempty
        skipLink
        navbar

jsPolyfillsLink :: FloraHTML
jsPolyfillsLink = do
  TemplateEnv{assets, environment} <- ask
  let jsPolyfillsURL = "/static/" <> assets.jsPolyfills.name
  case environment of
    Production ->
      script_ [src_ jsPolyfillsURL, type_ "module", defer_ "", integrity_ ("sha256-" <> assets.jsPolyfills.hash)] ("" :: Text)
    _ ->
      script_ [src_ jsPolyfillsURL, type_ "module", defer_ ""] ("" :: Text)

cssLink :: FloraHTML
cssLink = do
  TemplateEnv{assets, environment} <- ask
  let cssURL = "/static/" <> assets.cssBundle.name
  case environment of
    Production ->
      link_ [rel_ "stylesheet", href_ cssURL, integrity_ ("sha256-" <> assets.cssBundle.hash)]
    _ ->
      link_ [rel_ "stylesheet", href_ cssURL]

ogTags :: FloraHTML
ogTags = do
  TemplateEnv{title, description, environment, https, domain, httpPort} <- ask
  meta_ [property_ "og:title", content_ title]
  meta_ [property_ "og:site_name", content_ "Flora"]
  meta_ [property_ "og:description", content_ description]
  meta_ [property_ "og:url", content_ ""]
  meta_ [property_ "og:image", content_ (renderAbsoluteLink environment https domain httpPort "/static/og-image.png?v=1")]
  meta_ [property_ "og:image:width", content_ "1200"]
  meta_ [property_ "og:image:height", content_ "675"]
  meta_ [property_ "og:locale", content_ "en_GB"]
  meta_ [property_ "og:type", content_ "website"]
