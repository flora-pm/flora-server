{-# LANGUAGE QuasiQuotes #-}

module FloraWeb.Components.Header where

import Control.Monad.Reader
import Data.Text
import Lucid
import Lucid.Alpine
import PyF

import FloraWeb.Components.Navbar (navbar)
import FloraWeb.Components.Utils (property_, text)
import FloraWeb.Templates.Types (FloraHTML, TemplateEnv (..))

header :: FloraHTML
header = do
  TemplateEnv{title} <- ask
  doctype_
  html_
    [ lang_ "en"
    , class_ "no-js"
    , xBind_ "class" "darkMode ? 'dark' : ''"
    , xData_ "{ darkMode: localStorage.getItem('darkMode') !== 'false' }"
    , xInit_ "$watch('darkMode', val => localStorage.setItem('darkMode', val))"
    ]
    $ do
      head_ $ do
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        title_ (text title)

        script_ [type_ "module"] $ do
          toHtmlRaw @Text
            [str|
          document.documentElement.classList.remove('no-js');
          document.documentElement.classList.add('js');
          |]
        script_ [src_ "/static/js/app.js", type_ "module", defer_ ""] ("" :: Text)

        link_ [rel_ "stylesheet", href_ "/static/css/app.css"]
        meta_ [name_ "description", content_ "A package repository for the Haskell ecosystem"]
        ogTags
        theme
        link_ [rel_ "icon", href_ "/static/favicon.svg", type_ "image/svg+xml"]
        -- link_ [rel_ "canonical", href_ $ getCanonicalURL assigns]
        meta_ [name_ "twitter:dnt", content_ "on"]

      body_ [class_ "bg-background dark:bg-dark-2 dark:text-gray-100"] $ do
        navbar

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
