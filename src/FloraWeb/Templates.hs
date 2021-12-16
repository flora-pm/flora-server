module FloraWeb.Templates (render, mkErrorPage) where

import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader
import Data.ByteString.Lazy
import Lucid

import FloraWeb.Server.Auth
import FloraWeb.Templates.Layout.App (footer, header)
import FloraWeb.Templates.Types

render :: TemplateAssigns -> FloraHTML -> FloraPageM (Html ())
render ta template = pure $ toHtmlRaw $ runIdentity $
  runReaderT (renderBST (rendered template)) ta

mkErrorPage :: TemplateAssigns -> FloraHTML -> ByteString
mkErrorPage ta template = runIdentity $
  runReaderT (renderBST (rendered template)) ta

rendered :: FloraHTML -> FloraHTML
rendered target = do
  header
  target
  footer
