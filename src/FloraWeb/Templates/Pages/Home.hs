{-# LANGUAGE QuasiQuotes #-}
module FloraWeb.Templates.Pages.Home where

import CMarkGFM
import Lucid
import Lucid.Svg (d_, fill_, path_, stroke_, stroke_linecap_, stroke_linejoin_,
                  stroke_width_, viewBox_)
import PyF

import Data.Text (Text)
import FloraWeb.Templates.Types

show :: FloraHTML
show = main_ $ do
    banner
    searchBar

about :: FloraHTML
about = do
  div_ [ class_ "max-w-7xl py-16 px-4 sm:pb-4 sm:pt-24 sm:px-6 lg:px-8 about-page" ] $ do
    div_ [class_ "divider text-center sm:mb-16"] $ do
      div_ [class_ "text-center"] $ do
        p_ [class_ "mt-1 text-4xl font-extrabold dark:text-gray-100 sm:text-5xl sm:tracking-tight lg:text-6xl"] "Flora.pm"
        p_ [class_ "max-w-xl mt-5 mx-auto text-xl dark:text-gray-100"] "An index for the Haskell ecosystem"
    div_ [class_ ""] $ do
      aboutText

aboutText :: FloraHTML
aboutText = do
  toHtmlRaw $ commonmarkToHtml [optUnsafe] [] text
  where
    text :: Text
    text = [fmt|

<h3 class="font-bold text-xl dark:text-gray-100 mb-10"> What is Flora? </h3>
Flora.pm is a package index for the <a href="https://haskell.org">Haskell</a> ecosystem. It indexes packages from <a href="https://hackage.haskell.org">Hackage</a>,
and provides a number of new features and improvements:

* Better category model, with elimination of duplicates and curation
* Package namespaces, so that packages with the same name can live without conflict
* Beautiful package pages
* Responsive interface for mobile devices
* Dark mode

Flora is the work of volunteers, and the source can be read on <a href="https://github.com/flora-pm/flora-server">Github</a>.

<h3 class="font-bold text-xl dark:text-gray-100 my-10"> Moderation and Code of Conduct </h3>

The Flora project is governed by a [Code of Conduct](https://github.com/flora-pm/flora-server/blob/development/CODE_OF_CONDUCT.md).
If you feel like a resource on the service or a participant in the project has an inappropriate behaviour in relation to the code of conduct at [moderation@flora.pm](mailto:moderation@flora.pm).

|]

banner :: FloraHTML
banner = do
  div_ [class_ "relative"] $
    div_ [class_ "px-4 py-16 sm:px-6 sm:py-24 lg:py-16 lg:px-8"] $
      h1_ [class_ "text-center text-2xl font-extrabold tracking-tight sm:text-5xl lg:text-4xl"] $
        span_ [class_ "dark:text-gray-100 text-black headline"] "Search Haskell packages on Flora"

searchBar :: FloraHTML
searchBar =
  div_ [class_ "main-search max-w-md mx-auto flex justify-center rounded-xl border-2 overflow-hidden"] $ do
    input_ [ class_ "text-2xl text-gray-800 bg:bg-background dark:bg-background-dark dark:text-gray-300 block rounded-md border-0 focus-outline-none focus:ring-0 focus:brand-purple flex-grow p-2"
           , type_ "search", name_ "search", placeholder_ "Find a package", value_ "", tabindex_ "1"
           ]
    button_ [ type_ "submit", class_ "items-center right-0 top-0 mt-5 mr-4 mb-5"] $
      svg_ [ xmlns_ "http://www.w3.org/2000/svg", class_ "h-6 w-6 my-auto m-2", style_ "color: gray", fill_ "none", viewBox_ "0 0 24 24", stroke_ "currentColor"] $
        path_ [stroke_linecap_ "round", stroke_linejoin_ "round", stroke_width_ "2", d_ "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"]
