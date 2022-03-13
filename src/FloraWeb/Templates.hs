module FloraWeb.Templates
  ( render
  , renderUVerb
  , mkErrorPage
  , module Types
  ) where

import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader
import Data.ByteString.Lazy
import Lucid

import FloraWeb.Templates.Layout.App (header)
import FloraWeb.Templates.Types as Types

render :: (Monad m) => TemplateEnv -> FloraHTML -> m (Html ())
render env template = pure $ toHtmlRaw $ runIdentity $
  runReaderT (renderBST (rendered template)) env

renderUVerb :: TemplateEnv -> FloraHTML -> Html ()
renderUVerb env template = toHtmlRaw $ runIdentity $
  runReaderT (renderBST (rendered template)) env

mkErrorPage :: TemplateEnv -> FloraHTML -> ByteString
mkErrorPage env template = runIdentity $
  runReaderT (renderBST (rendered template)) env

rendered :: FloraHTML -> FloraHTML
rendered target = do
  header
  main_ [] target
