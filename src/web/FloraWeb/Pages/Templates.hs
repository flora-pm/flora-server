module FloraWeb.Pages.Templates
  ( render
  , mkErrorPage
  , renderPartial
  , module Types
  )
where

import Control.Monad.Extra (whenJust)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (ask, runReaderT)
import Data.ByteString.Lazy
import Data.Text.Display
import Lucid

import Flora.Environment.Env (DeploymentEnv (..))
import FloraWeb.Components.Alert qualified as Alert
import FloraWeb.Components.Header (header)
import FloraWeb.Pages.Templates.Types as Types

render :: Monad m => TemplateEnv -> FloraHTML -> m (Html ())
render env template =
  pure $ toHtmlRaw $ runIdentity $ runReaderT (renderBST (rendered template)) env

renderPartial :: Monad m => TemplateEnv -> FloraHTML -> m (Html ())
renderPartial env template = do
  pure $ toHtmlRaw $ runIdentity $ runReaderT (renderBST template) env

mkErrorPage :: TemplateEnv -> FloraHTML -> ByteString
mkErrorPage env template = runIdentity $ runReaderT (renderBST (rendered template)) env

rendered :: FloraHTML -> FloraHTML
rendered target = do
  TemplateEnv{flashInfo, flashError} <- ask
  header
  whenJust flashInfo $ \msg -> do
    Alert.info (display msg)
  whenJust flashError $ \msg -> do
    Alert.exception (display msg)
  main_ [class_ "container-fluid"] target
