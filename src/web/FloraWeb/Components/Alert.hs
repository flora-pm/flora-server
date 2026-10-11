module FloraWeb.Components.Alert where

import Data.Text (Text)
import Lucid

import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Pages.Templates.Types

info :: Text -> FloraHTML
info message =
  div_ [role_ "status", class_ "alert alert--info"] $ do
    Icons.information
    p_ [class_ "alert-message"] $
      toHtml message

exception :: Text -> FloraHTML
exception message =
  div_ [role_ "alert", class_ "alert alert--danger"] $ do
    Icons.exception
    p_ [class_ "alert-message"] $
      toHtml message
