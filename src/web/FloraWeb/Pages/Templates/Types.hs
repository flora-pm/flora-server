module FloraWeb.Pages.Templates.Types
  ( FloraHTML
  , FlashInfo (..)
  , mkInfo
  , FlashError (..)
  , mkError
  , TemplateEnv (..)
  , defaultsToEnv
  , fromSession
  , ActiveElements (..)
  , defaultTemplateEnv
  )
where

import Control.Monad.Identity
import Control.Monad.Reader (ReaderT)
import Data.Text (Text)
import Data.UUID qualified as UUID
import GHC.Generics
import Lucid
import Optics.Core

import Data.Text.Display
import Effectful
import Effectful.Reader.Static (Reader, ask)
import Flora.Environment
import Flora.Environment.Config (Assets)
import Flora.Model.PersistentSession (PersistentSessionId (..))
import Flora.Model.User
import FloraWeb.Common.Auth
import FloraWeb.Types

type FloraHTML = HtmlT (ReaderT TemplateEnv Identity) ()

newtype FlashInfo = FlashInfo {getFlashInfo :: Text}
  deriving (Show, Display) via Text

mkInfo :: Text -> FlashInfo
mkInfo = FlashInfo

newtype FlashError = FlashError {getFlashInfo :: Text}
  deriving (Show, Display) via Text

mkError :: Text -> FlashError
mkError = FlashError

data TemplateEnv = TemplateEnv
  { displayNavbarSearch :: Bool
  , flashInfo :: Maybe FlashInfo
  , flashError :: Maybe FlashError
  , title :: Text
  , mobileTitle :: Text
  , description :: Text
  , mUser :: Maybe User
  , sessionId :: PersistentSessionId
  , environment :: DeploymentEnv
  , features :: FeatureEnv
  , activeElements :: ActiveElements
  , assets :: Assets
  , indexPage :: Bool
  }
  deriving stock (Show, Generic)

-- | This record holds flags for UI elements that must be marked active in the templates.
data ActiveElements = ActiveElements
  { aboutNav :: Bool
  , packagesNav :: Bool
  , menuNav :: Bool
  , adminDashboard :: Bool
  }
  deriving stock (Show, Generic)

data TemplateDefaults = TemplateDefaults
  { displayNavbarSearch :: Bool
  , flashInfo :: Maybe FlashInfo
  , flashError :: Maybe FlashError
  , title :: Text
  , mobileTitle :: Text
  , description :: Text
  , mUser :: Maybe User
  , environment :: DeploymentEnv
  , features :: FeatureEnv
  , activeElements :: ActiveElements
  , indexPage :: Bool
  }
  deriving stock (Show, Generic)

defaultActiveElements :: ActiveElements
defaultActiveElements =
  ActiveElements
    { aboutNav = False
    , packagesNav = False
    , menuNav = False
    , adminDashboard = False
    }

defaultTemplateEnv :: TemplateDefaults
defaultTemplateEnv =
  TemplateDefaults
    { displayNavbarSearch = True
    , flashInfo = Nothing
    , flashError = Nothing
    , title = "Flora :: [Package]"
    , mobileTitle = "☰ Flora"
    , description = "Package index for the Haskell ecosystem"
    , mUser = Nothing
    , environment = Development
    , features = FeatureEnv Nothing
    , activeElements = defaultActiveElements
    , indexPage = True
    }

-- | ⚠  DO NOT USE THIS FUNCTION IF YOU DON'T KNOW WHAT YOU'RE DOING
defaultsToEnv :: Assets -> TemplateDefaults -> TemplateEnv
defaultsToEnv assets TemplateDefaults{..} =
  let sessionId = PersistentSessionId UUID.nil
   in TemplateEnv{..}

fromSession
  :: (Reader FeatureEnv :> es, IOE :> es)
  => Session
  -> TemplateDefaults
  -> Eff es TemplateEnv
fromSession session defaults = do
  let sessionId = session.sessionId
  let muser = session.mUser
  let webEnvStore = session.webEnvStore
  floraEnv <- liftIO $ fetchFloraEnv webEnvStore
  featuresEnv <- ask @FeatureEnv
  let assets = floraEnv.assets
  let TemplateDefaults{..} =
        defaults
          & (#mUser .~ muser)
          & (#environment .~ floraEnv.environment)
          & (#features .~ featuresEnv)
  pure TemplateEnv{..}
