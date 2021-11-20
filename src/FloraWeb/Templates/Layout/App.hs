module FloraWeb.Templates.Layout.App (header) where

import Control.Monad.Reader (ask)
import Data.Text
import FloraWeb.Templates.Types
import Lucid
import Lucid.Base

header :: FloraHTML
header = do
  assigns <- ask
  doctype_
  html_ [lang_ "en", class_ "no-js"] $ do
    head_ $ do
      meta_ [charset_ "UTF-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      title_ (text $ getTitle assigns)

      script_ [type_ "module"] $ do
        toHtmlRaw @Text "document.documentElement.classList.remove('no-js');"
        toHtmlRaw @Text "document.documentElement.classList.add('js');"

      link_ [rel_ "stylesheet", href_ "/static/css/app.css"]
      meta_ [name_ "description", content_ "A package repository for the Haskell ecosystem"]
      ogTags
      theme
      link_ [rel_ "icon", href_ "/favicon.ico"]
      link_ [rel_ "icon", href_ "/favicon.svg", type_ "image/svg+xml"]
      link_ [rel_ "canonical", href_ $ getCanonicalURL assigns]
      meta_ [name_ "twitter:dnt", content_ "on"]

    body_ $ do
      script_ [src_ "/static/js/app.js", type_ "module"] ("" :: Text)
      navBar

ogTags :: FloraHTML
ogTags = do
      assigns <- ask
      meta_ [property_ "og:title", content_ (getTitle assigns)]
      meta_ [property_ "og:site_name", content_ "Hex"]
      meta_ [property_ "og:description", content_ (getDescription assigns) ]
      meta_ [property_ "og:url", content_ (getCanonicalURL assigns)]
      meta_ [property_ "og:image", content_ (getImage assigns)]
      meta_ [property_ "og:image:width",  content_ "160"]
      meta_ [property_ "og:image:height", content_ "160"]
      meta_ [property_ "og:locale", content_ "en_GB"]
      meta_ [property_ "og:type", content_ "website"]

theme :: FloraHTML
theme = do
  meta_ [name_ "theme-color", content_ "#000", media_ "(prefers-color-scheme: dark)"]
  meta_ [name_ "theme-color", content_ "#FFF", media_ "(prefers-color-scheme: light)"]


navBar :: FloraHTML
navBar = do
  ta <- ask
  nav_ [class_ "navbar border-b border-gray-200"] $ do
    div_ [class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8"] $ do
      div_ [class_ "flex justify-between h-16"] $ do
        div_ [class_ "flex-shrink-0 flex items-center"] $
          a_ [href_ "/", class_ "dark:text-gray-900"] (getDisplayTitle ta)

        div_ [class_ "hidden margin-right flex sm:flex justify-end grid grid-rows-3 row-end-auto"] $ do
          a_ [href_ "#", class_ "navbar-element inline-flex items-center px-1 pt-1 border-b-2 mx-7"] "Packages"
          a_ [href_ "#", class_ "navbar-element inline-flex items-center px-1 pt-1 border-b-2 mx-7"] "Guides"
          a_ [href_ "#", class_ "navbar-element inline-flex items-center px-1 pt-1 border-b-2 mx-7"] "Login / Signup"

-- Helpers

property_ :: Text -> Attribute
property_ = makeAttribute "property"

text :: Text -> FloraHTML
text = toHtml

getTitle :: TemplateAssigns -> Text
getTitle ta = getTA ta "Flora" "title"

getDisplayTitle :: TemplateAssigns -> FloraHTML
getDisplayTitle ta = toHtml $ getTA ta "Flora :: [Package]" "display-title"

getDescription :: TemplateAssigns -> Text
getDescription ta = getTA ta "A package repository for the Haskell ecosystem" "description"

getCanonicalURL :: TemplateAssigns -> Text
getCanonicalURL ta = getTA ta "" "canonical_url"

getImage :: TemplateAssigns -> Text
getImage ta = getTA ta "" "image"
