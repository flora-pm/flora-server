{-# LANGUAGE IncoherentInstances #-}

module FloraWeb.Common.Guards where

import Data.Text (Text)
import Effectful
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Trace (Trace)
import Log qualified
import Monitor.Tracing qualified as Tracing
import Optics.Core

import Flora.Model.Package
import Flora.Model.PackageIndex.Query as Query
import Flora.Model.PackageIndex.Types (PackageIndex)
import Flora.Model.User (User)
import FloraWeb.Pages.Routes.Sessions
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Screens.Sessions qualified as Sessions
import FloraWeb.Session (Session)
import FloraWeb.Types (FloraEff)

guardThatPackageIndexExists
  :: (DB :> es, Trace :> es)
  => Namespace
  -> (Namespace -> Eff es PackageIndex)
  -- ^ Action to run if the package index does not exist
  -> Eff es PackageIndex
guardThatPackageIndexExists namespace action =
  Tracing.childSpan "guardThatPackageIndexExists " $ do
    result <-
      Tracing.childSpan "Query.getPackageIndexByName" $
        Query.getPackageIndexByName (extractNamespaceText namespace)
    case result of
      Just packageIndex -> pure packageIndex
      Nothing -> action namespace

guardThatUserHasProvidedTOTP
  :: Session (Maybe User)
  -> Maybe Text
  -> (Text -> FloraEff CreateSessionResult)
  -> FloraEff CreateSessionResult
guardThatUserHasProvidedTOTP session mTOTP totpAction = do
  case mTOTP of
    Just totp -> totpAction totp
    Nothing -> do
      Log.logInfo_ "User did not provide a TOTP code"
      templateDefaults <- templateFromSession session defaultTemplateEnv
      let templateEnv =
            templateDefaults
              & (#flashError ?~ mkError "Must provide an OTP code")
      body <- render templateEnv Sessions.newSession
      pure $ AuthenticationFailure body
