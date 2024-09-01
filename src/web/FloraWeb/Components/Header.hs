{-# LANGUAGE QuasiQuotes #-}

module FloraWeb.Components.Header where

import Control.Monad (unless)
import Control.Monad.Reader
import Data.Text (Text)
import Htmx.Lucid.Core (hxGet_, hxTrigger_)
import Lucid
import PyF

import Flora.Environment.Config
import FloraWeb.Components.Navbar (navbar)
import FloraWeb.Components.Utils
import FloraWeb.Pages.Templates.Types (FloraHTML, TemplateEnv (..))

header :: FloraHTML
header = do
  TemplateEnv{title, indexPage} <- ask
  doctype_
  html_
    [ lang_ "en"
    , class_ "no-js"
    , xData_
        "{ theme: \
        \ localStorage.getItem('theme') \
        \   || (window.matchMedia('(prefers-color-scheme: dark)').matches \
        \   ? 'dark' : 'light') \
        \ }"
    , xBind_ "data-theme" "(theme === 'dark') ? 'dark' : 'light'"
    , xInit_ "$watch('theme', val => localStorage.setItem('theme', val))"
    ]
    $ do
      head_ $ do
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        unless indexPage $ meta_ [name_ "robots", content_ "noindex"]
        -- link_ [rel_ "icon", href_ "/static/icons/favicon.svg", type_ "image/svg+xml"]
        link_ [rel_ "apple-touch-icon", sizes_ "180x180", href_ "/static/icons/apple-touch-icon.png"]
        link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "/static/icons/favicon-32x32.png"]
        link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "/static/icons/favicon-16x16.png"]
        link_ [rel_ "manifest", href_ "/static/icons/site.webmanifest"]
        link_ [rel_ "mask-icon", href_ "/static/icons/safari-pinned-tab.svg", color_ "#5bbad5"]
        meta_ [name_ "msapplication-TileColor", content_ "#da532c"]
        meta_ [name_ "theme-color", content_ "#ffffff"]

        title_ (text title)

        script_ [type_ "module"] $ do
          toHtmlRaw @Text
            [str|
          document.documentElement.classList.remove('no-js');
          document.documentElement.classList.add('js');
          |]

        jsLink
        cssLink
        meta_ [name_ "color-scheme", content_ "light dark"]
        link_
          [ rel_ "search"
          , type_ "application/opensearchdescription+xml"
          , title_ "Flora"
          , href_ "/opensearch.xml"
          ]
        meta_ [name_ "description", content_ "A package repository for the Haskell ecosystem"]
        ogTags
        theme
        -- link_ [rel_ "canonical", href_ $ getCanonicalURL assigns]
        meta_ [name_ "twitter:dnt", content_ "on"]

      body_ [] $ do
        div_ [hxGet_ "/livereload", hxTrigger_ "every 2s"] mempty
        navbar

jsLink :: FloraHTML
jsLink = do
  TemplateEnv{assets, environment} <- ask
  let jsURL = "/static/" <> assets.jsBundle.name
  case environment of
    Production ->
      script_ [src_ jsURL, type_ "module", defer_ "", integrity_ ("sha256-" <> assets.jsBundle.hash)] ("" :: Text)
    _ ->
      script_ [src_ jsURL, type_ "module", defer_ ""] ("" :: Text)

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
  TemplateEnv{title, description} <- ask
  meta_ [property_ "og:title", content_ title]
  meta_ [property_ "og:site_name", content_ "Flora"]
  meta_ [property_ "og:description", content_ description]
  meta_ [property_ "og:url", content_ ""]
  meta_ [property_ "og:image", content_ ""]
  meta_ [property_ "og:image:width", content_ "160"]
  meta_ [property_ "og:image:height", content_ "160"]
  meta_ [property_ "og:locale", content_ "en_GB"]
  meta_ [property_ "og:type", content_ "website"]

theme :: FloraHTML
theme = do
  meta_ [name_ "theme-color", content_ "#000", media_ "(prefers-color-scheme: dark)"]
  meta_ [name_ "theme-color", content_ "#FFF", media_ "(prefers-color-scheme: light)"]
