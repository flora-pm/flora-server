module FloraWeb.Templates.Pages.Home where

import FloraWeb.Templates.Layout.App (header)
import FloraWeb.Templates.Types
import Lucid
import Lucid.Base
import Lucid.Svg (d_, fill_, stroke_, stroke_linecap_, stroke_linejoin_,
                  stroke_width_, viewBox_)

show :: FloraHTML
show = do
  header
  main_ $ do
    banner
    searchBar

banner :: FloraHTML
banner = do
  div_ [class_ "relative"] $
    div_ [class_ "px-4 py-16 sm:px-6 sm:py-24 lg:py-16 lg:px-8"] $
      h1_ [class_ "text-center text-2xl font-extrabold tracking-tight sm:text-5xl lg:text-4xl"] $
        span_ [class_ "text-white headline"] "Search Haskell packages on Flora"

searchBar :: FloraHTML
searchBar =
  div_ [class_ "main-search max-w-md mx-auto text-gray-600 dark:text-gray-300 flex justify-center rounded-xl border-2 overflow-hidden"] $ do
    input_ [ class_ "block rounded-md border-0 focus-outline-none focus:ring-0 focus:border-blue-500 flex-grow p-2"
           , type_ "search", name_ "search", placeholder_ "Find a package", value_ "", tabindex_ "1"
           ]
    button_ [ type_ "submit", class_ "items-center right-0 top-0 mt-5 mr-4 mb-5"] $
      svg_ [ xmlns_ "http://www.w3.org/2000/svg", class_ "h-6 w-6 my-auto m-2", style_ "color: gray", fill_ "none", viewBox_ "0 0 24 24", stroke_ "currentColor"] $
        path_ [stroke_linecap_ "round", stroke_linejoin_ "round", stroke_width_ "2", d_ "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"]

path_ :: Applicative m => [Attribute] -> HtmlT m ()
path_ = with (makeElementNoEnd "path")
