module FloraWeb.Components.Utils where

import Data.Text (Text)
import FloraWeb.Templates.Types (FloraHTML)
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
