{-# LANGUAGE OverloadedRecordDot #-}

module FloraWeb.Pages.Server.Sessions where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Data.Text.Display
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time
import Log qualified
import Lucid (Html)
import Optics.Core
import RequireCallStack
import Sel.Hashing.Password qualified as Sel
import Servant (Headers (..), ServerT)

import Flora.Database
import Flora.Environment.Env
import Flora.Model.PersistentSession
import Flora.Model.User
import Flora.Model.User.Query qualified as Query
import Flora.Monad
import FloraWeb.Common.Auth
import FloraWeb.Common.Auth.TwoFactor qualified as TwoFactor
import FloraWeb.Common.Guards (guardThatUserHasProvidedTOTP)
import FloraWeb.Common.Utils
import FloraWeb.Pages.Routes.Sessions
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Screens.Sessions as Sessions
import FloraWeb.Session
import FloraWeb.Types (FloraEff)

server :: RequireCallStack => SessionWithCookies (Maybe User) -> ServerT Routes FloraEff
server s =
  Routes'
    { new = newSessionHandler s
    , create = createSessionHandler s
    , delete = deleteSessionHandler
    }

-- | Render the login page with a generic "Could not authenticate" flash error.
renderAuthFailure
  :: (IOE :> es, Reader FeatureEnv :> es)
  => Session (Maybe User)
  -> FloraM es (Html ())
renderAuthFailure session = do
  templateDefaults <- templateFromSession session defaultTemplateEnv
  let templateEnv = templateDefaults & (#flashError ?~ mkError "Could not authenticate")
  render templateEnv Sessions.newSession

newSessionHandler :: SessionWithCookies (Maybe User) -> FloraEff NewSessionResult
newSessionHandler (Headers session _) = do
  let mUser = session.user
  case mUser of
    Nothing -> do
      Log.logInfo_ "[+] No user logged-in"
      templateEnv' <- templateFromSession session defaultTemplateEnv
      let templateEnv =
            templateEnv'
              { title = "Login — Flora.pm"
              , description = "Login page"
              }
      html <- render templateEnv Sessions.newSession
      pure $ AuthenticationRequired html
    Just u -> do
      Log.logInfo_ $ "[+] User is already logged: " <> display u
      pure $ AlreadyAuthenticated "/"

createSessionHandler
  :: (IOE :> es, Reader FeatureEnv :> es, Reader FloraEnv :> es, Time :> es)
  => SessionWithCookies (Maybe User)
  -> LoginForm
  -> FloraM es CreateSessionResult
createSessionHandler (Headers session _) LoginForm{email, password, totp} = do
  FloraEnv{pool} <- Reader.ask
  mUser <- withReadOnlyPool pool $ Query.getUserByEmail email
  case mUser of
    Nothing -> do
      Log.logInfo_ "[+] Couldn't find user"
      body <- renderAuthFailure session
      pure $ AuthenticationFailure body
    Just user ->
      if user.userFlags.canLogin
        then
          if Sel.verifyText user.password password
            then do
              if user.totpEnabled
                then guardThatUserHasProvidedTOTP session totp $ \userCode -> checkTOTPIsValid session userCode user
                else do
                  sessionId <- withReadWritePool pool $ persistSession session.sessionId user.userId
                  let sessionCookie = craftSessionCookie sessionId True
                  pure $ AuthenticationSuccess ("/", sessionCookie)
            else do
              Log.logInfo_ "Invalid password"
              body <- renderAuthFailure session
              pure $ AuthenticationFailure body
        else do
          Log.logInfo_ "User not allowed to log-in"
          body <- renderAuthFailure session
          pure $ AuthenticationFailure body

checkTOTPIsValid
  :: (IOE :> es, Reader FeatureEnv :> es, Reader FloraEnv :> es, Time :> es)
  => Session (Maybe User)
  -> Text
  -> User
  -> FloraM es CreateSessionResult
checkTOTPIsValid session userCode user = do
  FloraEnv{pool} <- Reader.ask
  validated <- liftIO $ TwoFactor.validateTOTP (fromJust user.totpKey) userCode
  if validated
    then do
      Log.logInfo_ "[+] User connected!"
      sessionId <- withReadWritePool pool $ persistSession session.sessionId user.userId
      let sessionCookie = craftSessionCookie sessionId True
      pure $ AuthenticationSuccess ("/", sessionCookie)
    else do
      Log.logInfo_ "[+] Couldn't authenticate user's TOTP code"
      body <- renderAuthFailure session
      pure $ AuthenticationFailure body

deleteSessionHandler :: (IOE :> es, Reader FloraEnv :> es) => PersistentSessionId -> FloraM es DeleteSessionResponse
deleteSessionHandler sessionId = do
  FloraEnv{pool} <- Reader.ask
  Log.logInfo_ $ "[+] Logging-off session " <> display sessionId
  withReadWritePool pool $ deleteSession sessionId
  pure $ redirectWithCookie "/" emptySessionCookie
