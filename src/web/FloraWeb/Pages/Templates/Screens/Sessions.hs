module FloraWeb.Pages.Templates.Screens.Sessions where

import FloraWeb.Pages.Templates.Types
import Lucid

newSession :: FloraHTML
newSession = do
  let formClasses = "login-form"
  form_ [action_ "/sessions/new", method_ "POST", class_ formClasses] $ do
    h2_ [class_ ""] "Sign in"
    div_ $ do
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
    div_ $ do
      label_ [for_ "password", class_ "sr-only"] "Email address"
      input_
        [ id_ "password"
        , name_ "password"
        , type_ "password"
        , autocomplete_ "current-password"
        , required_ ""
        , placeholder_ "Password"
        , class_ "form-input"
        ]
    div_ $
      button_ [type_ "submit", class_ "btn bg-brand-purple text-white w-full my-2"] "Sign in"
