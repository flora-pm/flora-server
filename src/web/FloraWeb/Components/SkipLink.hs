module FloraWeb.Components.SkipLink where

import Lucid

import FloraWeb.Pages.Templates.Types

skipLink :: FloraHTML
skipLink =
  div_ [class_ "skipLink"] $ do
    a_ [class_ "btn", href_ "#main"] "Skip to main"

