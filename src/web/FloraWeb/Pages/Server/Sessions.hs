{-# LANGUAGE OverloadedRecordDot #-}

module FloraWeb.Pages.Server.Sessions where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Data.Text.Display
import Log qualified
import Optics.Core
import Sel.Hashing.Password qualified as Sel
import Servant

import Flora.Model.PersistentSession
import Flora.Model.User
import Flora.Model.User.Query qualified as Query
import FloraWeb.Common.Auth
import FloraWeb.Common.Auth.TwoFactor qualified as TwoFactor
import FloraWeb.Common.Guards (guardThatUserHasProvidedTOTP)
import FloraWeb.Common.Utils
import FloraWeb.Pages.Routes.Sessions
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Screens.Sessions as Sessions
import FloraWeb.Session
import FloraWeb.Types (FloraEff)

server :: SessionWithCookies (Maybe User) -> ServerT Routes FloraEff
server s =
  Routes'
    { new = newSessionHandler s
    , create = createSessionHandler s
    , delete = deleteSessionHandler
    }

newSessionHandler :: SessionWithCookies (Maybe User) -> FloraEff (Union NewSessionResponses)
newSessionHandler (Headers session _) = do
  let mUser = session.user
  case mUser of
    Nothing -> do
      Log.logInfo_ "[+] No user logged-in"
      templateDefaults <- templateFromSession session defaultTemplateEnv
      respond $ WithStatus @200 $ renderUVerb templateDefaults Sessions.newSession
    Just u -> do
      Log.logInfo_ $ "[+] User is already logged: " <> display u
      respond $ WithStatus @301 (redirect "/")

createSessionHandler
  :: SessionWithCookies (Maybe User)
  -> LoginForm
  -> FloraEff (Union CreateSessionResponses)
createSessionHandler (Headers session _) LoginForm{email, password, totp} = do
  mUser <- Query.getUserByEmail email
  case mUser of
    Nothing -> do
      Log.logInfo_ "[+] Couldn't find user"
      templateDefaults <- templateFromSession session defaultTemplateEnv
      let templateEnv =
            templateDefaults
              & (#flashError ?~ mkError "Could not authenticate")
      respond $ WithStatus @401 $ renderUVerb templateEnv Sessions.newSession
    Just user ->
      if user.userFlags.canLogin
        then
          if Sel.verifyText user.password password
            then do
              if user.totpEnabled
                then guardThatUserHasProvidedTOTP session totp $ \userCode -> do
                  checkTOTPIsValid session userCode user
                else do
                  sessionId <- persistSession session.sessionId user.userId
                  let sessionCookie = craftSessionCookie sessionId True
                  respond $ WithStatus @301 $ redirectWithCookie "/" sessionCookie
            else do
              Log.logInfo_ "Invalid password"
              templateDefaults <- templateFromSession session defaultTemplateEnv
              let templateEnv =
                    templateDefaults
                      & (#flashError ?~ mkError "Could not authenticate")
              respond $ WithStatus @401 $ renderUVerb templateEnv Sessions.newSession
        else do
          Log.logInfo_ "User not allowed to log-in"
          templateDefaults <- templateFromSession session defaultTemplateEnv
          let templateEnv =
                templateDefaults
                  & (#flashError ?~ mkError "Could not authenticate")
          respond $ WithStatus @401 $ renderUVerb templateEnv Sessions.newSession

checkTOTPIsValid
  :: Session (Maybe User)
  -> Text
  -> User
  -> FloraEff (Union CreateSessionResponses)
checkTOTPIsValid session userCode user = do
  validated <- liftIO $ TwoFactor.validateTOTP (fromJust user.totpKey) userCode
  if validated
    then do
      Log.logInfo_ "[+] User connected!"
      sessionId <- persistSession session.sessionId user.userId
      let sessionCookie = craftSessionCookie sessionId True
      respond $ WithStatus @301 $ redirectWithCookie "/" sessionCookie
    else do
      Log.logInfo_ "[+] Couldn't authenticate user's TOTP code"
      templateDefaults <- templateFromSession session defaultTemplateEnv
      let templateEnv =
            templateDefaults
              & (#flashError ?~ mkError "Could not authenticate")
      respond $ WithStatus @401 $ renderUVerb templateEnv Sessions.newSession

deleteSessionHandler :: PersistentSessionId -> FloraEff DeleteSessionResponse
deleteSessionHandler sessionId = do
  Log.logInfo_ $ "[+] Logging-off session " <> display sessionId
  deleteSession sessionId
  pure $ redirectWithCookie "/" emptySessionCookie
