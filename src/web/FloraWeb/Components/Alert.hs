module FloraWeb.Components.Alert where

import Data.Text (Text)
import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Pages.Templates.Types
import Lucid

info :: Text -> FloraHTML
info message =
  output_ [role_ "status", class_ "alert alert-info"] $ do
    Icons.information
    div_ [class_ "alert-message"] $
      toHtml message

exception :: Text -> FloraHTML
exception message =
  output_ [role_ "status", class_ "alert alert-error"] $ do
    Icons.exception
    div_ [class_ "alert-message"] $
      toHtml message
