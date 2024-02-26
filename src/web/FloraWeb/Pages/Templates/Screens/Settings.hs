module FloraWeb.Pages.Templates.Screens.Settings where

import Data.Text (Text)
import Data.Text.Display (display)
import Lucid

import Flora.Model.PersistentSession (PersistentSessionId (..))
import Flora.Model.User
import FloraWeb.Components.Button (button)
import FloraWeb.Pages.Templates

dashboard :: PersistentSessionId -> User -> FloraHTML
dashboard sessionId _user = main_ $
  div_ [class_ "container"] $ do
    div_ [class_ "divider"] $ do
      div_ [class_ "page-title"] $ do
        h1_ "Account settings"
    header_ [id_ "subheader"] $ do
      logOff sessionId
    section_ [class_ "settings_menu"] $ do
      ul_ [] $ do
        li_ $ a_ [href_ "/settings/profile"] "Profile"
        li_ $ a_ [href_ "/settings/security"] "Security"

logOff :: PersistentSessionId -> FloraHTML
logOff sessionId =
  form_ [action_ ("/sessions/delete/" <> display sessionId), method_ "post", id_ "logoff"] $ do
    let btnClasses = "font-bold inline-flex items-center py-3 mx-4 text-white dark:text-gray-100 "
    button_ [type_ "submit", class_ btnClasses] "Sign out"

profileSettings :: User -> FloraHTML
profileSettings _user = do
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

twoFactorSettings :: Text -> Text -> FloraHTML
twoFactorSettings qrCode base32Key = do
  div_ [class_ "container"] $ do
    div_ [class_ "divider"] $ do
      div_ [class_ "page-title"] $ do
        h1_ "Two-Factor Authentication"
      h2_ "Scan the QR Code"
    img_ [src_ ("data:image/png;base64," <> qrCode), height_ "300", width_ "300"]
    toHtml base32Key
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
