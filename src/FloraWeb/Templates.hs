module FloraWeb.Templates
  ( render
  , renderUVerb
  , mkErrorPage
  , module Types
  )
where

import Control.Monad (when)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Lazy
import Data.Text (Text)
import Lucid

import Flora.Environment (DeploymentEnv (..))
import FloraWeb.Components.Header (header)
import FloraWeb.Templates.Types as Types

render :: (Monad m) => TemplateEnv -> FloraHTML -> m (Html ())
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
rendered deploymentEnv target = do
  header
  main_ [] target
  when (deploymentEnv == Development) $
    script_ [src_ "/static/js/autoreload.js", type_ "module"] ("" :: Text)
