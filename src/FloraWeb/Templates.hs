module FloraWeb.Templates (render) where

import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader
import Lucid

import FloraWeb.Templates.Layout.App (header)
import FloraWeb.Templates.Types
import FloraWeb.Types

render :: TemplateAssigns -> FloraHTML -> FloraM (Html ())
render ta template = pure $ toHtmlRaw $ runIdentity $
  runReaderT (renderBST (rendered template)) ta

rendered :: FloraHTML -> FloraHTML
rendered target = do
  header
  target
  -- footer
