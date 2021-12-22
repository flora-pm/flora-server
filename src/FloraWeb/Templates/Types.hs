module FloraWeb.Templates.Types where

import Control.Monad.Identity
import Control.Monad.Reader
import Lucid
import GHC.Generics
import Data.Text (Text)

type FloraHTML = HtmlT (ReaderT TemplateEnv Identity) ()

newtype FlashInfo = FlashInfo { getFlashInfo :: Text }
  deriving Show via Text
newtype FlashError = FlashError { getFlashInfo :: Text }
  deriving Show via Text

data TemplateEnv = TemplateEnv
  { displayNavbarSearch :: Bool
  , flashInfo :: Maybe FlashInfo
  , flashError ::Maybe FlashError
  , title :: Text
  , description :: Text
  }
  deriving stock (Show, Generic)

defaultTemplateEnv :: TemplateEnv
defaultTemplateEnv = TemplateEnv
  { displayNavbarSearch = True
  , flashInfo = Nothing
  , flashError = Nothing
  , title = "Flora :: [Package]"
  , description = "Package index for the Haskell ecosystem"
  }
