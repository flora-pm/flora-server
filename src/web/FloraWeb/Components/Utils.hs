module FloraWeb.Components.Utils where

import Data.Text (Text)
import FloraWeb.Pages.Templates.Types (FloraHTML)
import Lucid (Attribute, a_, class_, href_, role_, toHtml)
import Lucid.Base (makeAttribute)

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
