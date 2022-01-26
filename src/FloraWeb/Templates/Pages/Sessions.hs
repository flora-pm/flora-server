module FloraWeb.Templates.Pages.Sessions where

import FloraWeb.Templates.Types
import Lucid

newSession :: FloraHTML
newSession =
  div_ [class_ "login-form max-w-md mx-auto flex justify-center"] $ do
    div_ [class_ "ml-5"] $ h2_ [class_ "mt-6 text-center text-3xl font-extrabold text-gray-900 dark:text-gray-100"] $ do
      "Sign in"
      form_ [class_ "mt-8 space-y-6", action_ "/sessions/new", method_ "POST"] $ do
        div_ [class_ "rounded-md shadow-sm -space-y-px"] $ do
          let fieldClass = "appearance-none rounded-none relative block w-full px-3 py-2 border border-gray-300 placeholder-gray-500 text-gray-900 rounded-t-md focus:outline-none focus:z-10 sm:text-sm"
          div_ $ do
            label_ [for_ "email", class_ "sr-only"] "Email address"
            input_ [ id_ "email", name_ "email", type_ "email", autocomplete_ "email", required_ ""
                   , placeholder_ "Email address" , class_ fieldClass ]
          div_ $ do
            label_ [for_ "password", class_ "sr-only"] "Email address"
            input_ [ id_ "password", name_ "password", type_ "password", autocomplete_ "current-password", required_ ""
                   , placeholder_ "Password" , class_ fieldClass ]
          div_ $ do
            label_ [for_ "remember", class_ "text-xl mr-3"] "Remember me"
            input_ [ id_ "remember", name_ "remember", type_ "checkbox", class_ "" ]

        div_ $
          button_ [type_ "submit", class_ "group relative w-full flex justify-center border border-transparent text-sm font-medium rounded-md text-white"] $
            span_ [class_ "absolute left-0 inset-y-0 flex items-center pl-3"] "Sign in"
