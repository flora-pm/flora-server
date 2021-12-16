module FloraWeb.Templates.Types
  ( FloraHTML
  , FlashInfo(..)
  , mkInfo
  , FlashError(..)
  , mkError
  , TemplateEnv(..)
  , fromSession
  , ActiveElements(..)
  , defaultTemplateEnv
  ) where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Text (Text)
import GHC.Generics
import Lucid
import Optics.Core

import Flora.Environment
import Flora.Model.PersistentSession (PersistentSessionId)
import Flora.Model.User
import FloraWeb.Server.Auth
import FloraWeb.Types

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
  , sessionId           :: PersistentSessionId
  , environment         :: DeploymentEnv
  , activeElements      :: ActiveElements
  }
  deriving stock (Show, Generic)

-- | This record holds flags for UI elements that must be marked active in the templates.
data ActiveElements = ActiveElements
  { aboutNav    :: Bool
  , packagesNav :: Bool
  , menuNav     :: Bool
  }
  deriving stock (Show, Generic)

data TemplateDefaults = TemplateDefaults
  { displayNavbarSearch :: Bool
  , flashInfo           :: Maybe FlashInfo
  , flashError          :: Maybe FlashError
  , title               :: Text
  , description         :: Text
  , mUser               :: Maybe User
  , environment         :: DeploymentEnv
  , activeElements      :: ActiveElements
  }
  deriving stock (Show, Generic)

defaultActiveElements :: ActiveElements
defaultActiveElements = ActiveElements
  { aboutNav = False
  , packagesNav = False
  , menuNav = False
  }

defaultTemplateEnv :: TemplateDefaults
defaultTemplateEnv = TemplateDefaults
  { displayNavbarSearch = True
  , flashInfo = Nothing
  , flashError = Nothing
  , title = "Flora :: [Package]"
  , description = "Package index for the Haskell ecosystem"
  , mUser = Nothing
  , environment = Development
  , activeElements = defaultActiveElements
  }

fromSession :: (MonadIO m) => Session -> TemplateDefaults -> m TemplateEnv
fromSession Session{sessionId, mUser=mu, webEnvStore} defaults = do
  floraEnv <- liftIO $ fetchFloraEnv webEnvStore
  let TemplateDefaults{..} = defaults & (#mUser .~ mu) & (#environment .~ (floraEnv ^. #environment))
  pure TemplateEnv{..}
