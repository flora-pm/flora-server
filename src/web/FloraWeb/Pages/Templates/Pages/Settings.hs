module FloraWeb.Pages.Templates.Pages.Settings where

import Lucid

import Data.Text (Text)
import Flora.Model.User
import FloraWeb.Components.Button (button)
import FloraWeb.Pages.Templates

-- import FloraWeb.Components.Button

dashboard :: User -> FloraHTML
dashboard user = main_ $
  div_ [class_ "container"] $ do
    div_ [class_ "divider"] $ do
      div_ [class_ "page-title"] $ do
        h1_ "Account settings"
    section_ [class_ "settings_menu"] $ do
      ul_ [] $ do
        li_ $ a_ [href_ "/settings/profile"] "Profile"
        li_ $ a_ [href_ "/settings/security"] "Security"

profileSettings :: User -> FloraHTML
profileSettings user = do
  div_ [class_ "container"] $ do
    div_ [class_ "divider"] $ do
      div_ [class_ "page-title"] $ do
        h1_ "Profile settings"
  section_ [] $ do
    form_ [] $ do
      h6_ [] "User information"
      div_ $ do
        label_ [for_ "email"] "Email address"
        input_ [type_ "text", name_ "email", id_ "email", required_ "", class_ "form-input"]
      div_ $
        button_ [type_ "submit", class_ ""] "Update profile"
      hr_ [class_ "settings_separator"]

securitySettings :: FloraHTML
securitySettings = do
  div_ [class_ "container"] $ do
    div_ [class_ "divider"] $ do
      div_ [class_ "page-title"] $ do
        h1_ "Security settings"
  section_ [] $ do
    ul_ [] $ do
      li_ [] $
        a_ [href_ "/settings/security/two-factor"] "Two-factor authentication"

twoFactorSettings :: Text -> FloraHTML
twoFactorSettings qrCode = do
  div_ [class_ "container"] $ do
    div_ [class_ "divider"] $ do
      div_ [class_ "page-title"] $ do
        h1_ "Two-Factor Authentication"
      h2_ "Scan the QR Code"
    img_ [src_ ("data:image/png;base64," <> qrCode), height_ "300", width_ "300"]
    form_ [action_ "/settings/security/two-factor/setup", method_ "POST"] $ do
      label_ [for_ "code"] "Code from the authenticator app"
      input_
        [ id_ "code"
        , name_ "code"
        , type_ "text"
        , required_ ""
        , placeholder_ "XXXXXX"
        , class_ "form-input"
        ]
      button "Save"

twoFactorSettingsRemove :: FloraHTML
twoFactorSettingsRemove = do
  div_ [class_ "container"] $ do
    div_ [class_ "divider"] $ do
      div_ [class_ "page-title"] $ do
        h1_ "Two-Factor Authentication"
    form_ [action_ "/settings/security/two-factor/delete", method_ "POST"] $
      button "Delete authenticator application"
