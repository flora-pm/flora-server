{-# LANGUAGE RecordWildCards #-}

module FloraWeb.Pages.Templates.Types
  ( FloraHTML
  , FlashInfo (..)
  , mkInfo
  , FlashError (..)
  , mkError
  , TemplateEnv (..)
  , defaultsToEnv
  , ActiveElements (..)
  , defaultTemplateEnv
  , FromSession (..)
  )
where

import Control.Monad.Identity
import Control.Monad.Reader (ReaderT)
import Data.Text (Text)
import Data.Text.Display
import Data.UUID qualified as UUID
import Data.Word (Word16)
import Effectful
import Effectful.Reader.Static (Reader, ask)
import GHC.Generics
import Lucid
import Optics.Core

import Flora.Environment.Config (Assets)
import Flora.Environment.Env
import Flora.Model.PersistentSession (PersistentSessionId (..))
import Flora.Model.User
import FloraWeb.Common.Auth
import FloraWeb.Types

type FloraHTML = HtmlT (ReaderT TemplateEnv Identity) ()

newtype FlashInfo = FlashInfo {getFlashInfo :: Text}
  deriving (Display, Show) via Text

mkInfo :: Text -> FlashInfo
mkInfo = FlashInfo

newtype FlashError = FlashError {getFlashInfo :: Text}
  deriving (Display, Show) via Text

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
  , navbarSearchContent :: Maybe Text
  , domain :: Text
  , httpPort :: Word16
  }
  deriving stock (Generic, Show)

-- | This record holds flags for UI elements that must be marked active in the templates.
data ActiveElements = ActiveElements
  { aboutNav :: Bool
  , packagesNav :: Bool
  , menuNav :: Bool
  , adminDashboard :: Bool
  }
  deriving stock (Generic, Show)

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
  , navbarSearchContent :: Maybe Text
  }
  deriving stock (Generic, Show)

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
    , navbarSearchContent = Nothing
    }

-- | ⚠  DO NOT USE THIS FUNCTION IF YOU DON'T KNOW WHAT YOU'RE DOING
defaultsToEnv :: FloraEnv -> TemplateDefaults -> TemplateEnv
defaultsToEnv floraEnv TemplateDefaults{..} =
  let assets = floraEnv.assets
      sessionId = PersistentSessionId UUID.nil
      domain = floraEnv.domain
      httpPort = floraEnv.httpPort
   in TemplateEnv{..}

class FromSession a where
  templateFromSession :: (IOE :> es, Reader FeatureEnv :> es) => a -> TemplateDefaults -> Eff es TemplateEnv

instance FromSession (Session User) where
  templateFromSession session defaults = do
    let sessionId = session.sessionId
    let muser = Just session.user
    let webEnvStore = session.webEnvStore
    floraEnv <- liftIO $ fetchFloraEnv webEnvStore
    featuresEnv <- ask @FeatureEnv
    let assets = floraEnv.assets
    let domain = floraEnv.domain
        httpPort = floraEnv.httpPort
    let TemplateDefaults{..} =
          defaults
            & (#mUser .~ muser)
            & (#environment .~ floraEnv.environment)
            & (#features .~ featuresEnv)
    pure TemplateEnv{..}

instance FromSession (Session (Maybe User)) where
  templateFromSession session defaults = do
    let sessionId = session.sessionId
    let muser = session.user
    let webEnvStore = session.webEnvStore
    floraEnv <- liftIO $ fetchFloraEnv webEnvStore
    featuresEnv <- ask @FeatureEnv
    let assets = floraEnv.assets
    let domain = floraEnv.domain
        httpPort = floraEnv.httpPort
    let TemplateDefaults{..} =
          defaults
            & (#mUser .~ muser)
            & (#environment .~ floraEnv.environment)
            & (#features .~ featuresEnv)
    pure TemplateEnv{..}
