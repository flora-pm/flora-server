module FloraWeb.Pages.Templates
  ( render
  , renderUVerb
  , mkErrorPage
  , module Types
  )
where

import Control.Monad.Extra (whenJust)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (ask, runReaderT)
import Data.ByteString.Lazy
import Data.Text.Display
import Lucid

import Flora.Environment (DeploymentEnv (..))
import FloraWeb.Components.Alert qualified as Alert
import FloraWeb.Components.Header (header)
import FloraWeb.Pages.Templates.Types as Types

render :: Monad m => TemplateEnv -> FloraHTML -> m (Html ())
render env template = pure (renderUVerb env template)

renderUVerb :: TemplateEnv -> FloraHTML -> Html ()
renderUVerb env template =
  let deploymentEnv = env.environment
   in toHtmlRaw $ runIdentity $ runReaderT (renderBST (rendered deploymentEnv template)) env

mkErrorPage :: TemplateEnv -> FloraHTML -> ByteString
mkErrorPage env template =
  let deploymentEnv = env.environment
   in runIdentity $ runReaderT (renderBST (rendered deploymentEnv template)) env

rendered :: DeploymentEnv -> FloraHTML -> FloraHTML
rendered _deploymentEnv target = do
  TemplateEnv{flashInfo, flashError} <- ask
  header
  whenJust flashInfo $ \msg -> do
    Alert.info (display msg)
  whenJust flashError $ \msg -> do
    Alert.exception (display msg)
  main_ [] target

-- when (deploymentEnv == Development) $
--   script_ [src_ "/static/js/autoreload.js", type_ "module"] ("" :: Text)
