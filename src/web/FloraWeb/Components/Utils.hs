module FloraWeb.Components.Utils where

import Data.Text (Text)
import Data.Text qualified as Text
import Lucid
import Lucid.Base (makeAttributes)

import FloraWeb.Pages.Templates.Types (FloraHTML)

text :: Text -> FloraHTML
text = toHtml

property_ :: Text -> Attributes
property_ = makeAttributes "property"

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

-- Standard WAI-ARIA attributes for accessibility purpose
ariaLabel_ :: Text -> Attributes
ariaLabel_ = makeAttributes "aria-label"

-- Prefer these ones as they are integrated with AlpineJS
ariaControls_ :: Text -> Attributes
ariaControls_ = makeAttributes ":aria-controls"

ariaExpanded_ :: Text -> Attributes
ariaExpanded_ = makeAttributes ":aria-expanded"

xId_ :: Text -> Attributes
xId_ = makeAttributes "x-id"

id'_ :: Text -> Attributes
id'_ = makeAttributes ":id"

-- | @datalist@ element
dataText_ :: Text -> Attributes
dataText_ = makeAttributes "data-text"

hxSseConnect_ :: Text -> Attributes
hxSseConnect_ = makeAttributes "sse-connect"

hxSseSwap_ :: Text -> Attributes
hxSseSwap_ = makeAttributes "sse-swap"

color_ :: Text -> Attributes
color_ = makeAttributes "color"

xData_ :: Text -> Attributes
xData_ = makeAttributes "x-data"

-- | x-on
-- Listen for browser events on an element
xOn_
  :: Text
  -- ^ Event name
  -> Text
  -> Attributes
xOn_ event = makeAttributes ("x-on:" <> event)

-- | x-show
-- Toggle the visibility of an element
xShow_ :: Text -> Attributes
xShow_ = makeAttributes "x-show"

-- | x-bind
-- Dynamically set HTML attributes on an element
xBind_
  :: Text
  -- ^ Attribute name
  -> Text
  -> Attributes
xBind_ attr = makeAttributes ("x-bind:" <> attr)

-- | x-init
-- Run code when an element is initialized by Alpine
xInit_ :: Text -> Attributes
xInit_ = makeAttributes "x-init"

-- | x-model
-- Synchronize a piece of data with an input element
xModel_
  :: [Text]
  -- ^ List of x-model modifiers
  -> Text
  -> Attributes
xModel_ mods = case mods of
  [] -> makeAttributes "x-model"
  _ -> makeAttributes ("x-model." <> Text.intercalate "." mods)
