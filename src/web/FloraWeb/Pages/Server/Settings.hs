module FloraWeb.Pages.Server.Settings
  ( Routes
  , server
  ) where

import Control.Monad.IO.Class
import Data.ByteString.Base32 qualified as Base32
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
import FloraWeb.Types (FloraEff)

server :: ServerT Routes FloraEff
server =
  Routes'
    { index = userSettingsHandler
    , getSecuritySettings = userSecuritySettingsHandler
    , getTwoFactorSettings = getTwoFactorSettingsHandler
    , postTwoFactorSetup = postTwoFactorSetupHandler
    , deleteTwoFactorSetup = deleteTwoFactorSetupHandler
    }

userSettingsHandler :: SessionWithCookies User -> FloraEff (Html ())
userSettingsHandler (Headers session _) = do
  let user = session.user
  templateEnv' <- templateFromSession session defaultTemplateEnv
  let templateEnv =
        templateEnv'
          & #title
          .~ "Account settings"
  render templateEnv $
    Settings.dashboard session.sessionId user

userSecuritySettingsHandler :: SessionWithCookies User -> FloraEff (Html ())
userSecuritySettingsHandler (Headers session _) = do
  templateEnv' <- templateFromSession session defaultTemplateEnv
  let templateEnv =
        templateEnv'
          & #title
          .~ "Security settings"
  render
    templateEnv
    Settings.securitySettings

getTwoFactorSettingsHandler :: SessionWithCookies User -> FloraEff (Html ())
getTwoFactorSettingsHandler (Headers session _) = do
  let user = session.user
  FloraEnv{domain} <- getEnv session
  templateEnv' <- templateFromSession session defaultTemplateEnv
  let templateEnv =
        templateEnv'
          & #title
          .~ "Security settings"
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

postTwoFactorSetupHandler :: SessionWithCookies User -> TwoFactorConfirmationForm -> FloraEff (Union TwoFactorSetupResponses)
postTwoFactorSetupHandler (Headers session _) TwoFactorConfirmationForm{code = userCode} = do
  let user = session.user
  templateEnv' <- templateFromSession session defaultTemplateEnv
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
                  & #title
                  .~ "Security settings"
                  & #flashError
                  ?~ mkError "Code validation failed, please retry"
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

deleteTwoFactorSetupHandler :: SessionWithCookies User -> FloraEff DeleteTwoFactorSetupResponse
deleteTwoFactorSetupHandler (Headers session _) = do
  let user = session.user
  Update.unSetTOTP user.userId
  pure $ redirect "/settings/security"
