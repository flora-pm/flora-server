{-# LANGUAGE IncoherentInstances #-}

module FloraWeb.Common.Guards where

import Data.Text (Text)
import Effectful
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Trace (Trace)
import FloraWeb.Pages.Templates
import Log qualified
import Monitor.Tracing qualified as Tracing
import Optics.Core
import Servant (respond)
import Servant.API.UVerb

import Flora.Model.Package
import Flora.Model.PackageIndex.Query as Query
import Flora.Model.PackageIndex.Types (PackageIndex)
import Flora.Model.User (User)
import FloraWeb.Pages.Routes.Sessions (CreateSessionResponses)
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
  -> (Text -> FloraEff (Union CreateSessionResponses))
  -> FloraEff (Union CreateSessionResponses)
guardThatUserHasProvidedTOTP session mTOTP action = do
  case mTOTP of
    Just totp -> action totp
    Nothing -> do
      Log.logInfo_ "User did not provide a TOTP code"
      templateDefaults <- templateFromSession session defaultTemplateEnv
      let templateEnv =
            templateDefaults
              & (#flashError ?~ mkError "Must provide an OTP code")
      respond $ WithStatus @401 $ renderUVerb templateEnv Sessions.newSession
