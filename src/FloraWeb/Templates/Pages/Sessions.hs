module FloraWeb.Templates.Pages.Sessions where

import FloraWeb.Templates.Types
import Lucid

newSession :: FloraHTML
newSession = do
  let formClasses = "login-form max-w-md mx-auto justify-center px-2 mt-12"
  form_ [action_ "/sessions/new", method_ "POST", class_ formClasses] $ do
    h2_ [class_ "text-center text-3xl mb-3 font-extrabold text-gray-900 dark:text-gray-100"] "Sign in"
    div_ $ do
      label_ [for_ "email", class_ "sr-only"] "Email address"
      input_
        [ id_ "email"
        , name_ "email"
        , type_ "email"
        , autocomplete_ "email"
        , required_ ""
        , placeholder_ "Email address"
        , class_ "form-input block w-full"
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
        , class_ "form-input block w-full"
        ]
    div_ $ do
      label_ [for_ "remember", class_ "text-xl mr-3"] "Remember me"
      input_ [id_ "remember", name_ "remember", type_ "checkbox", class_ ""]

    div_ $
      button_ [type_ "submit", class_ "btn bg-brand-purple text-white w-full my-2"] "Sign in"
