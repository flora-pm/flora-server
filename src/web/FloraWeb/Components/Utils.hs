module FloraWeb.Components.Utils where

import Data.Text (Text)
import FloraWeb.Templates.Types (FloraHTML)
import Lucid
import Lucid.Base (makeAttribute)
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

text :: Text -> FloraHTML
text = toHtml

property_ :: Text -> Attribute
property_ = makeAttribute "property"

data LinkOptions = LinkOptions
  { href :: Text
  , classes :: Text
  , childNode :: FloraHTML
  }

link :: LinkOptions -> FloraHTML
link LinkOptions{href, classes, childNode} =
  a_
    [class_ classes, role_ "link", href_ href]
    childNode

defaultLinkOptions :: LinkOptions
defaultLinkOptions =
  LinkOptions
    { href = ""
    , classes = ""
    , childNode = mempty
    }

-- Prefer these ones as they are integrated with AlpineJS
ariaControls_ :: Text -> Attribute
ariaControls_ = makeAttribute ":aria-controls"

ariaExpanded_ :: Text -> Attribute
ariaExpanded_ = makeAttribute ":aria-expanded"

xId_ :: Text -> Attribute
xId_ = makeAttribute "x-id"

id'_ :: Text -> Attribute
id'_ = makeAttribute ":id"

data SearchBarOptions = SearchBarOptions
  { actionUrl :: Text
  , placeholder :: Text
  }

searchBar :: SearchBarOptions -> FloraHTML
searchBar SearchBarOptions{actionUrl, placeholder} =
  form_ [action_ actionUrl, method_ "GET"] $! do
    div_ [class_ "main-search"] $! do
      label_ [for_ "search"] ""
      input_
        [ class_
            "search-bar"
        , type_ "search"
        , id_ "search"
        , name_ "q"
        , placeholder_ placeholder
        , value_ ""
        , tabindex_ "1"
        , autofocus_
        ]
      button_ [type_ "submit"] $
        svg_ [xmlns_ "http://www.w3.org/2000/svg", style_ "color: gray", fill_ "none", viewBox_ "0 0 24 24", stroke_ "currentColor"] $
          path_ [stroke_linecap_ "round", stroke_linejoin_ "round", stroke_width_ "2", d_ "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"]
