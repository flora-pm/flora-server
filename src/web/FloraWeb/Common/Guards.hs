{-# LANGUAGE IncoherentInstances #-}

module FloraWeb.Common.Guards where

import Data.Text (Text)
import Distribution.Types.Version (Version)
import Effectful
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Time (Time)
import FloraWeb.Pages.Templates
import Log qualified
import Optics.Core
import Servant (respond)
import Servant.API.UVerb

import Flora.Model.Package
import Flora.Model.Package.Query qualified as Query
import Flora.Model.PackageIndex.Query as Query
import Flora.Model.PackageIndex.Types (PackageIndex)
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types (Release)
import FloraWeb.Common.Auth
import FloraWeb.Pages.Routes.Sessions (CreateSessionResponses)
import FloraWeb.Pages.Templates.Pages.Sessions qualified as Sessions
import FloraWeb.Session (getSession)

guardThatPackageExists
  :: (DB :> es, Log :> es, Time :> es)
  => Namespace
  -> PackageName
  -> (Namespace -> PackageName -> Eff es Package)
  -- ^ Action to run if the package does not exist
  -> Eff es Package
guardThatPackageExists namespace packageName action = do
  result <- Query.getPackageByNamespaceAndName namespace packageName
  case result of
    Nothing -> action namespace packageName
    Just package ->
      case package.status of
        FullyImportedPackage -> pure package
        UnknownPackage -> action namespace packageName

guardThatReleaseExists
  :: DB :> es
  => PackageId
  -> Version
  -> (Version -> Eff es Release)
  -- ^ Action to run if the package does not exist
  -> Eff es Release
guardThatReleaseExists packageId version action = do
  result <- Query.getReleaseByVersion packageId version
  case result of
    Just release -> pure release
    Nothing -> action version

guardThatPackageIndexExists
  :: DB :> es
  => Namespace
  -> (Namespace -> Eff es PackageIndex)
  -- ^ Action to run if the package index does not exist
  -> Eff es PackageIndex
guardThatPackageIndexExists namespace action = do
  result <- Query.getPackageIndexByName (extractNamespaceText namespace)
  case result of
    Just packageIndex -> pure packageIndex
    Nothing -> action namespace

guardThatUserHasProvidedTOTP
  :: Maybe Text
  -> (Text -> FloraPage (Union CreateSessionResponses))
  -> FloraPage (Union CreateSessionResponses)
guardThatUserHasProvidedTOTP mTOTP action = do
  case mTOTP of
    Just totp -> action totp
    Nothing -> do
      session <- getSession
      Log.logInfo_ "User did not provide a TOTP code"
      templateDefaults <- fromSession session defaultTemplateEnv
      let templateEnv =
            templateDefaults
              & (#flashError ?~ mkError "Must provide an OTP code")
      respond $ WithStatus @401 $ renderUVerb templateEnv Sessions.newSession
