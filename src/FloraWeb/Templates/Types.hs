module FloraWeb.Templates.Types where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Text (Text)
import GHC.Generics
import Lucid
import Optics.Core

import Flora.Environment
import Flora.Model.User
import FloraWeb.Server.Auth

type FloraHTML = HtmlT (ReaderT TemplateEnv Identity) ()

newtype FlashInfo = FlashInfo { getFlashInfo :: Text }
  deriving Show via Text

mkInfo :: Text -> FlashInfo
mkInfo = FlashInfo

newtype FlashError = FlashError { getFlashInfo :: Text }
  deriving Show via Text

mkError :: Text -> FlashError
mkError = FlashError

data TemplateEnv = TemplateEnv
  { displayNavbarSearch :: Bool
  , flashInfo           :: Maybe FlashInfo
  , flashError          :: Maybe FlashError
  , title               :: Text
  , description         :: Text
  , mUser               :: Maybe User
  , environment         :: DeploymentEnv
  }
  deriving stock (Show, Generic)

defaultTemplateEnv :: TemplateEnv
defaultTemplateEnv = TemplateEnv
  { displayNavbarSearch = True
  , flashInfo = Nothing
  , flashError = Nothing
  , title = "Flora :: [Package]"
  , description = "Package index for the Haskell ecosystem"
  , mUser = Nothing
  , environment = Development
  }

fromSession :: Session -> TemplateEnv
fromSession Session{mUser, floraEnv} = defaultTemplateEnv{mUser, environment = floraEnv ^. #environment}
