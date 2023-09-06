{-# LANGUAGE QuasiQuotes #-}

module FloraWeb.Pages.Templates.Pages.Home where

import CMarkGFM
import Control.Monad.Reader
import Data.Text (Text)
import Lucid
import Lucid.Svg
  ( d_
  , fill_
  , path_
  , stroke_
  , stroke_linecap_
  , stroke_linejoin_
  , stroke_width_
  , viewBox_
  )
import PyF

import Flora.Environment
import FloraWeb.Pages.Templates.Types

show :: FloraHTML
show = do
  banner
  searchBar
  buttons

about :: FloraHTML
about = do
  TemplateEnv{environment} <- ask
  div_ [class_ "about-page"] $ do
    div_ [class_ "divider about-page__banner"] $ do
      p_ [class_ "about-page__title"] "Flora.pm"
      p_ [class_ "about-page__subtitle"] "An index for the Haskell ecosystem"
    case environment of
      Development ->
        p_ [class_ ""] "⚠ You are using a development instance of Flora ⚠"
      _ -> ""
    aboutText

aboutText :: FloraHTML
aboutText = do
  toHtmlRaw $ commonmarkToHtml [optUnsafe] [] text
  where
    text :: Text
    text =
      [str|
<h3 class=""> What is Flora? </h3>

<div class="bullets">

Flora.pm is a package index for the [Haskell](https://haskell.org) ecosystem. It indexes packages from [Hackage](https://hackage.haskell.org)
and provides new features and improvements:

* Curated category model, with elimination of duplicates
* Package namespaces, so that packages with the same name can live without conflict
* Beautiful package pages
* Responsive interface for mobile devices
* Dark mode
</div>

Flora is the work of volunteers, and the source can be read on [GitHub](https://github.com/flora-pm/flora-server).

<h3> Moderation and Code of Conduct </h3>

The Flora project is governed by a [Code of Conduct](https://github.com/flora-pm/flora-server/blob/development/CODE_OF_CONDUCT.md).
If you feel like a resource on the service or a participant in the project has an inappropriate behaviour in relation to the code of conduct,
please contact [moderation@flora.pm](mailto:moderation@flora.pm).

|]

banner :: FloraHTML
banner = do
  div_ [class_ "relative"] $
    h1_ [class_ "main-title"] $
      span_ [class_ "main-title"] "Search Haskell packages on Flora"

searchBar :: FloraHTML
searchBar =
  form_ [action_ "/search", method_ "GET"] $ do
    div_ [class_ "main-search"] $ do
      label_ [for_ "search"] ""
      input_
        [ class_
            "search-bar"
        , type_ "search"
        , id_ "search"
        , name_ "q"
        , placeholder_ "Find a package"
        , value_ ""
        , tabindex_ "1"
        , autofocus_
        ]
      button_ [type_ "submit"] $
        svg_ [xmlns_ "http://www.w3.org/2000/svg", style_ "color: gray", fill_ "none", viewBox_ "0 0 24 24", stroke_ "currentColor"] $
          path_ [stroke_linecap_ "round", stroke_linejoin_ "round", stroke_width_ "2", d_ "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"]

buttons :: FloraHTML
buttons =
  section_ [id_ "main-page-buttons"] $ do
    a_ [class_ "button", href_ "https://www.haskell.org/ghcup/"] $ do
      h2_
        [class_ "category-card__name"]
        "Install Haskell"
    a_ [class_ "button", href_ "https://cabal.readthedocs.io/en/stable/intro.html"] $ do
      h2_
        [class_ "category-card__name"]
        "Start with Cabal"
