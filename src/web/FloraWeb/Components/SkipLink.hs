module FloraWeb.Components.SkipLink where

import Lucid

import FloraWeb.Pages.Templates.Types

skipLink :: FloraHTML
skipLink =
  a_ [class_ "skipLink", href_ "#main"] "Skip to main"
