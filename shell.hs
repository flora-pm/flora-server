{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Lucid
import Lucid.Supplemental
import System.IO (stdout, hSetEncoding, utf8)
import Data.Text.Lazy.IO as L

main :: IO ()
main = do
  hSetEncoding stdout utf8
  L.hPutStr stdout (renderText template1)


-- Template for file: shell.html
template1 :: Html ()
template1 = do
  toHtmlRaw  "<!-- This example requires Tailwind CSS v2.0+ -->"
  toHtmlRaw  "<!--\n  This example requires updating your template:\n\n  ```\n  <html class=\"h-full\">\n  <body class=\"h-full\">\n  ```\n-->"
  div_ [ class_ "min-h-full" ] $ do
    nav_ [ class_ "bg-white border-b border-gray-200" ] $ do
      div_ [ class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ] $ div_ [ class_ "flex justify-between h-16" ] $ do
        div_ [ class_ "flex" ] $ do
          div_ [ class_ "flex-shrink-0 flex items-center" ] $ do
            img_ [ class_ "block lg:hidden h-8 w-auto", src_ "https://tailwindui.com/img/logos/workflow-mark-indigo-600.svg", alt_ "Workflow" ]
            img_ [ class_ "hidden lg:block h-8 w-auto", src_ "https://tailwindui.com/img/logos/workflow-logo-indigo-600-mark-gray-800-text.svg", alt_ "Workflow" ]
          div_ [ class_ "hidden sm:-my-px sm:ml-6 sm:flex sm:space-x-8" ] $ do
            toHtmlRaw  "<!-- Current: \"border-indigo-500 text-gray-900\", Default: \"border-transparent text-gray-500 hover:border-gray-300 hover:text-gray-700\" -->"
            a_ [ href_ "#", class_ "border-indigo-500 text-gray-900 inline-flex items-center px-1 pt-1 border-b-2 text-sm font-medium" ] $ "\n              Dashboard\n            "
            a_ [ href_ "#", class_ "border-transparent text-gray-500 hover:border-gray-300 hover:text-gray-700 inline-flex items-center px-1 pt-1 border-b-2 text-sm font-medium" ] $ "\n              Team\n            "
            a_ [ href_ "#", class_ "border-transparent text-gray-500 hover:border-gray-300 hover:text-gray-700 inline-flex items-center px-1 pt-1 border-b-2 text-sm font-medium" ] $ "\n              Projects\n            "
            a_ [ href_ "#", class_ "border-transparent text-gray-500 hover:border-gray-300 hover:text-gray-700 inline-flex items-center px-1 pt-1 border-b-2 text-sm font-medium" ] $ "\n              Calendar\n            "
        div_ [ class_ "hidden sm:ml-6 sm:flex sm:items-center" ] $ do
          button_ [ 