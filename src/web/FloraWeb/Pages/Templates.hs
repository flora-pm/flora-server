module FloraWeb.Pages.Templates
  ( render
  , mkErrorPage
  , module Types
  )
where

import Control.Monad.Extra (whenJust)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (ask, runReaderT)
import Data.ByteString.Lazy
import Data.Text (Text)
import Data.Text.Display
import Lucid

import Flora.Environment.Config
import FloraWeb.Components.Alert qualified as Alert
import FloraWeb.Components.Header (header)
import FloraWeb.Pages.Templates.Types as Types

render :: Monad m => TemplateEnv -> FloraHTML -> m (Html ())
render env template = do
  let deploymentEnv = env.environment
  pure $ toHtmlRaw $ runIdentity $ runReaderT (renderBST (rendered deploymentEnv template)) env

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
  main_ [class_ "container-fluid"] target
  prismLink

prismLink :: FloraHTML
prismLink = do
  TemplateEnv{assets, environment} <- ask
  let prismURL = "/static/" <> assets.prism.name
  case environment of
    Production ->
      script_ [src_ prismURL, type_ "module", defer_ "", integrity_ ("sha256-" <> assets.prism.hash)] ("" :: Text)
    _ ->
      script_ [src_ prismURL, type_ "module", defer_ ""] ("" :: Text)
