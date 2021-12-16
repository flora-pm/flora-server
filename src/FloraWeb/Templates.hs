module FloraWeb.Templates (render, mkErrorPage) where

import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader
import Data.ByteString.Lazy
import Lucid

import FloraWeb.Server.Auth
import FloraWeb.Templates.Layout.App (footer, header)
import FloraWeb.Templates.Types
import FloraWeb.Session

render :: TemplateEnv -> FloraHTML -> FloraPageM (Html ())
render env template = pure $ toHtmlRaw $ runIdentity $
  runReaderT (renderBST (rendered template)) env

mkErrorPage :: TemplateEnv -> FloraHTML -> ByteString
mkErrorPage env template = runIdentity $
  runReaderT (renderBST (rendered template)) env

rendered :: FloraHTML -> FloraHTML
rendered target = do
  header
  main_ [] target
  footer
