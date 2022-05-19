module FloraWeb.Components.Utils where

import Data.Text (Text)
import FloraWeb.Templates.Types (FloraHTML)
import Lucid (Attribute, toHtml)
import Lucid.Base (makeAttribute)

text :: Text -> FloraHTML
text = toHtml

property_ :: Text -> Attribute
property_ = makeAttribute "property"
