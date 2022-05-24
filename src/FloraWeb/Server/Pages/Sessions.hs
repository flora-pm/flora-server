module FloraWeb.Server.Pages.Sessions where

import Control.Monad.Reader
import Data.Password.Argon2
import Data.Text.Display
import qualified Log
import Optics.Core

import Database.PostgreSQL.Entity.DBT
import Flora.Environment
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
import FloraWeb.Types
import Servant

server :: ServerT Routes FloraPageM
server =
  Routes'
    { new = newSessionHandler
    , create = createSessionHandler
    , delete = deleteSessionHandler
    }

newSessionHandler :: FloraPageM (Union NewSessionResponses)
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

createSessionHandler :: LoginForm -> FloraPageM (Union CreateSessionResponses)
createSessionHandler LoginForm{email, password} = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  mUser <- liftIO $ withPool pool $ Query.getUserByEmail email
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
          sessionId <- persistSession pool (session ^. #sessionId) (user ^. #userId)
          let sessionCookie = craftSessionCookie sessionId True
          respond $
            WithStatus @301 $
              redirectWithCookie "/" sessionCookie
        else do
          Log.logInfo_ "[+] Couldn't authenticate user"
          templateDefaults <- fromSession session defaultTemplateEnv
          let templateEnv =
                templateDefaults
                  & (#flashError ?~ mkError "Could not authenticate")
          respond $ WithStatus @401 $ renderUVerb templateEnv Sessions.newSession

deleteSessionHandler :: PersistentSessionId -> FloraPageM DeleteSessionResponse
deleteSessionHandler sessionId = do
  Log.logInfo_ $ "[+] Logging-off session " <> display sessionId
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  liftIO $ withPool pool $ deleteSession sessionId
  pure $ redirectWithCookie "/" emptySessionCookie
