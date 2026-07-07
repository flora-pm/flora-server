module Data.Text.HTML
  ( TextHtml
  , fromText
  , toText
  ) where

import Control.DeepSeq
import Data.Aeson
import Data.Text
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import GHC.Generics
import Lucid
import Text.HTML.SanitizeXSS (sanitizeBalance)

-- | a wrapper that attaches from and tofield instances
--  for a text db row for LucidHtml
newtype TextHtml = MkTextHtml Text
  deriving stock (Generic, Show)
  deriving
    (Eq)
    via Text

instance ToHtml TextHtml where
  toHtml (MkTextHtml t) = toHtml (sanitizeBalance t)
  toHtmlRaw (MkTextHtml t) = toHtmlRaw (sanitizeBalance t)

instance ToJSON TextHtml where
  toJSON (MkTextHtml a) = String a

instance FromJSON TextHtml where
  parseJSON = withText "TextHtml" (pure . MkTextHtml)

instance NFData TextHtml where
  rnf a = seq a ()

--
instance FromField TextHtml where
  fromField field bs = MkTextHtml <$> fromField field bs

instance ToField TextHtml where
  toField (MkTextHtml x) = toField x

fromText :: Text -> TextHtml
fromText = MkTextHtml . sanitizeBalance

toText :: TextHtml -> Text
toText (MkTextHtml t) = t
