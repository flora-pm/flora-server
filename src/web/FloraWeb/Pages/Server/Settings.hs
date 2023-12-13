module FloraWeb.Pages.Server.Settings
  ( Routes
  , server
  ) where

import Control.Monad.IO.Class
import Data.ByteString.Base32 qualified as Base32
import Data.Maybe (fromJust)
import Data.Text.Encoding qualified as Text
import Log qualified
import Lucid
import Optics.Core
import Sel.HMAC.SHA256 qualified as HMAC
import Servant

import Flora.Environment
import Flora.Model.User
import Flora.Model.User.Update qualified as Update
import Flora.QRCode qualified as QRCode
import FloraWeb.Common.Auth.TwoFactor qualified as TwoFactor
import FloraWeb.Common.Utils (redirect)
import FloraWeb.Pages.Routes.Settings
import FloraWeb.Pages.Templates (render, renderUVerb)
import FloraWeb.Pages.Templates.Screens.Settings qualified as Settings
import FloraWeb.Pages.Templates.Types
import FloraWeb.Session

server :: ServerT Routes FloraPage
server =
  Routes'
    { index = userSettingsHandler
    , getSecuritySettings = userSecuritySettingsHandler
    , getTwoFactorSettings = getTwoFactorSettingsHandler
    , postTwoFactorSetup = postTwoFactorSetupHandler
    , deleteTwoFactorSetup = deleteTwoFactorSetupHandler
    }

userSettingsHandler :: FloraPage (Html ())
userSettingsHandler = do
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  let templateEnv =
        templateEnv'
          & #title .~ "Account settings"
  let user = fromJust session.mUser
  render templateEnv $
    Settings.dashboard user

userSecuritySettingsHandler :: FloraPage (Html ())
userSecuritySettingsHandler = do
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  let templateEnv =
        templateEnv'
          & #title .~ "Security settings"
  render
    templateEnv
    Settings.securitySettings

getTwoFactorSettingsHandler :: FloraPage (Html ())
getTwoFactorSettingsHandler = do
  FloraEnv{domain} <- getEnv
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  let templateEnv =
        templateEnv'
          & #title .~ "Security settings"
  let user = fromJust session.mUser
  case user.totpKey of
    Nothing -> do
      userKey <- liftIO HMAC.newAuthenticationKey
      Update.setupTOTP user.userId userKey
      let uri = TwoFactor.uriFromKey domain user.email userKey
      let qrCode =
            QRCode.generateQRCode uri
              & Text.decodeUtf8
      render templateEnv $
        Settings.twoFactorSettings
          qrCode
          (Base32.encodeBase32Unpadded $ HMAC.unsafeAuthenticationKeyToBinary userKey)
    Just userKey -> do
      if user.totpEnabled
        then render templateEnv Settings.twoFactorSettingsRemove
        else do
          let uri = TwoFactor.uriFromKey domain user.email userKey
          let qrCode =
                QRCode.generateQRCode uri
                  & Text.decodeUtf8
          render templateEnv $
            Settings.twoFactorSettings
              qrCode
              (Base32.encodeBase32Unpadded $ HMAC.unsafeAuthenticationKeyToBinary userKey)

postTwoFactorSetupHandler :: TwoFactorConfirmationForm -> FloraPage (Union TwoFactorSetupResponses)
postTwoFactorSetupHandler TwoFactorConfirmationForm{code = userCode} = do
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  let user = fromJust session.mUser
  case user.totpKey of
    Nothing -> respond $ WithStatus @301 (redirect "/settings/security/two-factor")
    Just userKey -> do
      validated <- liftIO $ TwoFactor.validateTOTP userKey userCode
      if validated
        then do
          Update.confirmTOTP user.userId
          Log.logInfo_ "Code validation succeeded"
          respond $ WithStatus @301 (redirect "/settings/security/two-factor")
        else do
          Log.logAttention_ "Code validation failed"
          let templateEnv =
                templateEnv'
                  & #title .~ "Security settings"
                  & #flashError ?~ mkError "Code validation failed, please retry"
          let uri = TwoFactor.uriFromKey "localhost" user.email userKey
          let qrCode =
                QRCode.generateQRCode uri
                  & Text.decodeUtf8
          respond $
            WithStatus @200 $
              renderUVerb templateEnv $
                Settings.twoFactorSettings
                  qrCode
                  (Base32.encodeBase32Unpadded $ HMAC.unsafeAuthenticationKeyToBinary userKey)

deleteTwoFactorSetupHandler :: FloraPage DeleteTwoFactorSetupResponse
deleteTwoFactorSetupHandler = do
  session <- getSession
  let user = fromJust session.mUser
  Update.unSetTOTP user.userId
  pure $ redirect "/settings/security"
