module FloraWeb.Components.MainSearchBar where

import Lucid

import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Components.Utils
import FloraWeb.Pages.Templates.Types (FloraHTML)

mainSearchBar :: FloraHTML
mainSearchBar =
  form_ [class_ "wrapper wrapper--small wrapper--nogutter", action_ "/search", method_ "GET"] $ do
    label_ [for_ "search", class_ "sr-only"] "Search a package"
    div_ [class_ "cluster cluster--nowrap cluster--stretch cluster--tiny flex-grow"] $ do
      input_
        [ class_ "input--big flex-grow min-w0"
        , id_ "search"
        , type_ "search"
        , name_ "q"
        , placeholder_ "Search a package"
        , autocomplete_ "off"
        , autocorrect_ "off"
        , autocapitalize_ "off"
        , spellcheck_ "off"
        ]
      button_ [class_ "btn btn--big", type_ "submit", ariaLabel_ "Search"] Icons.lookingGlass

