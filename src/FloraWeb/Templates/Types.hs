module FloraWeb.Templates.Types where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text
import Lucid

type TemplateEnv = ReaderT TemplateAssigns Identity

type FloraHTML = HtmlT TemplateEnv ()

newtype TemplateAssigns = TemplateAssigns { getAssigns :: Map Text Text }
  deriving newtype (Show, Eq)

newtype UserAssigns = UserAssigns {getUserAssigns :: Map Text Text}
  deriving newtype (Show, Eq)

emptyAssigns :: TemplateAssigns
emptyAssigns = TemplateAssigns Map.empty

mkAssigns :: TemplateAssigns -> Maybe UserAssigns -> TemplateAssigns
mkAssigns (TemplateAssigns templateAssigns) (Just (UserAssigns userAssigns)) =
  TemplateAssigns $ Map.union templateAssigns userAssigns
mkAssigns ta Nothing = ta

getTA :: TemplateAssigns -- ^  Template Assigns
      -> Text -- ^ Default value if the key is not present
      -> Text -- ^ Key
      -> Text
getTA (TemplateAssigns templateAssigns) defaultValue key = Map.findWithDefault defaultValue key templateAssigns
