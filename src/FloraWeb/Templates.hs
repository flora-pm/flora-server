{-# LANGUAGE CPP #-}

module FloraWeb.Templates
  ( render
  , renderUVerb
  , mkErrorPage
  , module Types
  )
where

import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader
import Data.ByteString.Lazy
import Lucid

import Data.Text (Text)
import Flora.Environment (DeploymentEnv (..))
import FloraWeb.Templates.Layout.App (header)
import FloraWeb.Templates.Types as Types
import Optics.Core

render :: (Monad m) => TemplateEnv -> FloraHTML -> m (Html ())
render env template =
  let deploymentEnv = env ^. #environment
   in pure $ toHtmlRaw $ runIdentity $ runReaderT (renderBST (rendered deploymentEnv template)) env

renderUVerb :: TemplateEnv -> FloraHTML -> Html ()
renderUVerb env template =
  let deploymentEnv = env ^. #environment
   in toHtmlRaw $ runIdentity $ runReaderT (renderBST (rendered deploymentEnv template)) env

mkErrorPage :: TemplateEnv -> FloraHTML -> ByteString
mkErrorPage env template =
  let deploymentEnv = env ^. #environment
   in runIdentity $ runReaderT (renderBST (rendered deploymentEnv template)) env

rendered :: DeploymentEnv -> FloraHTML -> FloraHTML
rendered deploymentEnv target = do
  header
  main_ [] target
  if deploymentEnv == Development
    then script_ [src_ "/static/js/autoreload.js", type_ "module"] ("" :: Text)
    else pure ()
