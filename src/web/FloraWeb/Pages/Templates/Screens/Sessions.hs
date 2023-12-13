module FloraWeb.Pages.Templates.Screens.Sessions where

import FloraWeb.Pages.Templates.Types
import Lucid

newSession :: FloraHTML
newSession = do
  let formClasses = "login-form"
  form_ [action_ "/sessions/new", method_ "POST", class_ formClasses] $ do
    h2_ [class_ ""] "Sign in"
    label_ [for_ "email", class_ "sr-only"] "Email address"
    input_
      [ id_ "email"
      , name_ "email"
      , type_ "email"
      , autocomplete_ "email"
      , required_ ""
      , placeholder_ "Email address"
      , class_ "form-input"
      ]
    label_ [for_ "password", class_ "sr-only"] "Email address"
    input_
      [ id_ "password"
      , name_ "password"
      , type_ "password"
      , autocomplete_ "current-password"
      , required_ ""
      , placeholder_ "Password"
      , class_ "form-input password"
      ]
    label_ [for_ "use_totp"] "Use two-factor authentication"
    input_
      [ id_ "use_totp"
      , name_ "use_totp"
      , type_ "checkbox"
      ]
    div_ [class_ "totp-zone"] $ do
      label_ [for_ "totp"] "Two-factor code"
      input_
        [ id_ "totp"
        , name_ "totp"
        , type_ "text"
        , pattern_ "0-9]+"
        , autocomplete_ "off"
        , class_ "form-input"
        ]
    div_ [class_ "login-button"] $
      button_ [type_ "submit"] "Sign in"
