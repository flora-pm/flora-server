module FloraWeb.Components.SlimSearchBar (slimSearchBar, SearchBarOptions (..)) where

import Data.Text (Text)
import Lucid

import FloraWeb.Components.Icons
import FloraWeb.Pages.Templates.Types (FloraHTML)

data SearchBarOptions = SearchBarOptions
  { actionUrl :: Text
  , placeholder :: Text
  , value :: Text
  }

slimSearchBar :: SearchBarOptions -> FloraHTML
slimSearchBar SearchBarOptions{actionUrl, placeholder, value} =
  form_ [action_ actionUrl, method_ "GET"] $! do
    div_ [class_ "secondary-search"] $ do
      label_ [for_ "search"] ""
      input_
        [ class_
            "search-bar"
        , type_ "search"
        , id_ "search"
        , name_ "q"
        , placeholder_ placeholder
        , value_ value
        , tabindex_ "1"
        , autofocus_
        ]
      button_ [class_ "search-btn", type_ "submit"] $
        lookingGlass
