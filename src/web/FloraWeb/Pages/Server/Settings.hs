module FloraWeb.Pages.Server.Settings
  ( Routes
  , server
  ) where

import Control.Monad.IO.Class
import Data.ByteString.Base32 qualified as Base32
import Data.Text.Encoding qualified as Text
import Effectful (Eff, IOE, (:>))
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (Reader)
import Effectful.Time (Time)
import Log qualified
import Lucid
import Optics.Core
import Sel.HMAC.SHA256 qualified as HMAC
import Servant (HasServer (..), Headers (..))

import Flora.Environment.Env
import Flora.Model.User
import Flora.Model.User.Update qualified as Update
import Flora.QRCode qualified as QRCode
import FloraWeb.Common.Auth.TwoFactor qualified as TwoFactor
import FloraWeb.Common.Utils (redirect)
import FloraWeb.Pages.Routes.Settings
import FloraWeb.Pages.Templates (render)
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

userSettingsHandler :: (IOE :> es, Reader FeatureEnv :> es) => SessionWithCookies User -> Eff es (Html ())
userSettingsHandler (Headers session _) = do
  let user = session.user
  templateEnv' <- templateFromSession session defaultTemplateEnv
  let templateEnv =
        templateEnv'
          & #title
          .~ "Account settings"
  render templateEnv $
    Settings.dashboard session.sessionId user

userSecuritySettingsHandler :: (IOE :> es, Reader FeatureEnv :> es) => SessionWithCookies User -> Eff es (Html ())
userSecuritySettingsHandler (Headers session _) = do
  templateEnv' <- templateFromSession session defaultTemplateEnv
  let templateEnv =
        templateEnv'
          & #title
          .~ "Security settings"
  render
    templateEnv
    Settings.securitySettings

getTwoFactorSettingsHandler
  :: ( DB :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Time :> es
     )
  => SessionWithCookies User
  -> Eff es (Html ())
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

postTwoFactorSetupHandler
  :: ( DB :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Time :> es
     )
  => SessionWithCookies User
  -> TwoFactorConfirmationForm
  -> Eff es TwoFactorSetupResult
postTwoFactorSetupHandler (Headers session _) TwoFactorConfirmationForm{code = userCode} = do
  let user = session.user
  templateEnv' <- templateFromSession session defaultTemplateEnv
  case user.totpKey of
    Nothing -> pure $ TwoFactorSetupNotEnabled "/settings/security/two-factor"
    Just userKey -> do
      validated <- liftIO $ TwoFactor.validateTOTP userKey userCode
      if validated
        then do
          Update.confirmTOTP user.userId
          Log.logInfo_ "Code validation succeeded"
          pure $ TwoFactorSetupSuccess "/settings/security/two-factor"
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
          body <-
            render templateEnv $
              Settings.twoFactorSettings
                qrCode
                (Base32.encodeBase32Unpadded $ HMAC.unsafeAuthenticationKeyToBinary userKey)
          pure $ TwoFactorSetupFailure body

deleteTwoFactorSetupHandler :: (DB :> es, Time :> es) => SessionWithCookies User -> Eff es DeleteTwoFactorSetupResponse
deleteTwoFactorSetupHandler (Headers session _) = do
  let user = session.user
  Update.unSetTOTP user.userId
  pure $ redirect "/settings/security"
