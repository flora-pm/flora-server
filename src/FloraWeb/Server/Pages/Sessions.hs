module FloraWeb.Server.Pages.Sessions where

import Data.Password.Argon2
import Data.Text.Display
import qualified Log
import Optics.Core

import Flora.Model.PersistentSession
import Flora.Model.User
import Flora.Model.User.Orphans ()
import qualified Flora.Model.User.Query as Query
import FloraWeb.Routes.Pages.Sessions
import FloraWeb.Server.Auth
import FloraWeb.Server.Utils
import FloraWeb.Session
import FloraWeb.Templates
import FloraWeb.Templates.Pages.Sessions as Sessions
import Servant

server :: ServerT Routes FloraPage
server =
  Routes'
    { new = newSessionHandler
    , create = createSessionHandler
    , delete = deleteSessionHandler
    }

newSessionHandler :: FloraPage (Union NewSessionResponses)
newSessionHandler = do
  session <- getSession
  let mUser = session ^. #mUser
  case mUser of
    Nothing -> do
      Log.logInfo_ "[+] No user logged-in"
      templateDefaults <- fromSession session defaultTemplateEnv
      respond $ WithStatus @200 $ renderUVerb templateDefaults Sessions.newSession
    Just u -> do
      Log.logInfo_ $ "[+] User is already logged: " <> display u
      respond $ WithStatus @301 (redirect "/")

createSessionHandler :: LoginForm -> FloraPage (Union CreateSessionResponses)
createSessionHandler LoginForm{email, password} = do
  session <- getSession
  mUser <- Query.getUserByEmail email
  case mUser of
    Nothing -> do
      Log.logInfo_ "[+] Couldn't find user"
      templateDefaults <- fromSession session defaultTemplateEnv
      let templateEnv =
            templateDefaults
              & (#flashError ?~ mkError "Could not authenticate")
      respond $ WithStatus @401 $ renderUVerb templateEnv Sessions.newSession
    Just user ->
      if validatePassword (mkPassword password) (user ^. #password)
        then do
          Log.logInfo_ "[+] User connected!"
          sessionId <- persistSession (session ^. #sessionId) (user ^. #userId)
          let sessionCookie = craftSessionCookie sessionId True
          respond $ WithStatus @301 $ redirectWithCookie "/" sessionCookie
        else do
          Log.logInfo_ "[+] Couldn't authenticate user"
          templateDefaults <- fromSession session defaultTemplateEnv
          let templateEnv =
                templateDefaults
                  & (#flashError ?~ mkError "Could not authenticate")
          respond $ WithStatus @401 $ renderUVerb templateEnv Sessions.newSession

deleteSessionHandler :: PersistentSessionId -> FloraPage DeleteSessionResponse
deleteSessionHandler sessionId = do
  Log.logInfo_ $ "[+] Logging-off session " <> display sessionId
  deleteSession sessionId
  pure $ redirectWithCookie "/" emptySessionCookie
