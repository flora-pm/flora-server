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

-- | a wrapper that attaches from and tofield instances
--  for a text db row for LucidHtml
newtype TextHtml = MkTextHtml Text
  deriving stock (Show, Generic)
  deriving
    (Eq, ToHtml)
    via Text

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
fromText = MkTextHtml

toText :: TextHtml -> Text
toText (MkTextHtml t) = t
