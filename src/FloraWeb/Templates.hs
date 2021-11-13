module FloraWeb.Templates where

import Control.Monad.Reader
import Lucid

import Control.Monad.Identity (runIdentity)
import FloraWeb.Templates.Types
import FloraWeb.Types

render :: TemplateAssigns -> FloraHTML -> FloraM (Html ())
render ta template = pure $ toHtmlRaw $ runIdentity $
  runReaderT (renderBST template) ta
