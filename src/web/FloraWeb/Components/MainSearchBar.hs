module FloraWeb.Components.MainSearchBar where

import Lucid

import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Pages.Templates.Types (FloraHTML)

mainSearchBar :: FloraHTML
mainSearchBar =
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
      button_ [class_ "search-btn", type_ "submit"] Icons.lookingGlass
    p_ [class_ "search-hint"] $ do
      Icons.lightbulb
      "Use " <> a_ [href_ "/documentation/search-features"] "search modifiers" <> " to enhance your queries"
