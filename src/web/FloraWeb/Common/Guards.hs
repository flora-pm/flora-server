{-# LANGUAGE IncoherentInstances #-}

module FloraWeb.Common.Guards where

import Data.Text (Text)
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Trace (Trace)
import Log qualified
import Monitor.Tracing qualified as Tracing
import Optics.Core

import Flora.Database
import Flora.Environment.Env
import Flora.Model.Package.Types
import Flora.Model.PackageIndex.Query as Query
import Flora.Model.PackageIndex.Types (PackageIndex)
import Flora.Model.User (User)
import Flora.Monad
import FloraWeb.Pages.Routes.Sessions
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Screens.Sessions qualified as Sessions
import FloraWeb.Session (Session)

guardThatPackageIndexExists
  :: (IOE :> es, Reader FloraEnv :> es, Trace :> es)
  => Namespace
  -> (Namespace -> FloraM es PackageIndex)
  -- ^ Action to run if the package index does not exist
  -> FloraM es PackageIndex
guardThatPackageIndexExists namespace action =
  Tracing.childSpan "guardThatPackageIndexExists " $ do
    FloraEnv{pool} <- Reader.ask
    result <-
      Tracing.childSpan "Query.getPackageIndexByName" $
        withReadOnlyPool pool $
          Query.getPackageIndexByName (extractNamespaceText namespace)
    case result of
      Just packageIndex -> pure packageIndex
      Nothing -> action namespace

guardThatUserHasProvidedTOTP
  :: (IOE :> es, Reader FeatureEnv :> es)
  => Session (Maybe User)
  -> Maybe Text
  -> (Text -> FloraM es CreateSessionResult)
  -> FloraM es CreateSessionResult
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
