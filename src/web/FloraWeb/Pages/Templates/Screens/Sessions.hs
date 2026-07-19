module FloraWeb.Pages.Templates.Screens.Sessions where

import Control.Monad.Reader
import Data.Text (Text)
import Lucid

import Flora.Environment.Config
import FloraWeb.Components.Utils
import FloraWeb.Pages.Templates.Types

jsAlpineLink :: FloraHTML
jsAlpineLink = do
  TemplateEnv{assets, environment} <- ask
  let jsAlpineURL = "/static/" <> assets.jsAlpine.name
  case environment of
    Production ->
      script_ [src_ jsAlpineURL, type_ "module", defer_ "", integrity_ ("sha256-" <> assets.jsAlpine.hash)] ("" :: Text)
    _ ->
      script_ [src_ jsAlpineURL, type_ "module", defer_ ""] ("" :: Text)

newSession :: FloraHTML
newSession = do
  let formClasses = "login-form"
  header_ [class_ "pageHead"] $ do
    div_ [class_ "wrapper aside"] $ do
      div_ [class_ "flow"] $ do
        h1_ [class_ "pageHead-title text-break text-center"] "Login"
  section_ [class_ "wrapper wrapper--small inset-large flow flow--large", id_ "content"] $ do
    form_
      [ class_ "flow"
      , action_ "/sessions/new"
      , method_ "POST"
      , class_ formClasses
      ]
      $ do
        div_ [class_ "flow flow--small"] $ do
          label_ [class_ "sr-only", for_ "email"] "Email address"
          input_
            [ id_ "email"
            , name_ "email"
            , type_ "email"
            , autocomplete_ "email"
            , required_ ""
            , placeholder_ "Email address"
            , class_ "w100"
            ]
        div_ [class_ "flow flow--small"] $ do
          label_ [class_ "sr-only", for_ "password"] "Email address"
          input_
            [ id_ "password"
            , name_ "password"
            , type_ "password"
            , autocomplete_ "current-password"
            , required_ ""
            , placeholder_ "Password"
            , class_ "w100 password"
            ]
        details_ [class_ "details--nobody flow flow--small"] $ do
          summary_ [] $ "Use two-factor authentication"
          label_ [class_ "sr-only", for_ "totp"] "Two-factor code"
          input_
            [ id_ "totp"
            , name_ "totp"
            , type_ "text"
            , pattern_ "0-9]+"
            , autocomplete_ "off"
            , placeholder_ "Two-factor code"
            , class_ "w100"
            ]
        div_ [class_ "flow flow--small"] $ do
          button_ [class_ "btn btn--big w100", type_ "submit"] "Log In"
  jsAlpineLink
