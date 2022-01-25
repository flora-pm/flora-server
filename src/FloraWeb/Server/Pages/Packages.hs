{-# LANGUAGE LambdaCase #-}
module FloraWeb.Server.Pages.Packages
  ( Routes
  , server
  ) where

import Control.Monad.Reader
import Data.Foldable
import Data.Function
import Data.Text
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Orphans ()
import Network.HTTP.Types.Status
import Optics.Core
import Servant.API.Generic
import Servant.Server.Generic

import qualified Data.Text as T
import Distribution.Parsec (simpleParsec)
import Distribution.Types.Version (Version)
import Flora.Environment
import Flora.Model.Package
import Flora.Model.Package.Types
import Flora.Model.Release
import Flora.Model.Requirement
import FloraWeb.Routes.Pages.Packages
import FloraWeb.Server.Auth
import FloraWeb.Session
import FloraWeb.Templates
import FloraWeb.Templates.Error
import qualified FloraWeb.Templates.Pages.Packages as Packages
import FloraWeb.Types

server :: ToServant Routes' (AsServerT FloraPageM)
server = genericServerT Routes'
  { show = showHandler
  , showVersion = showVersionHandler
  }

showHandler :: Text -> Text -> FloraPageM (Html ())
showHandler namespaceText nameText = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  case (validateNamespace namespaceText, validateName nameText) of
    (Just namespace, Just name) -> do
      result <- liftIO $ withPool pool $ getPackageByNamespaceAndName namespace name
      case result of
        Nothing -> renderError notFound404
        Just package -> do
          dependents <- liftIO $ withPool pool $ getPackageDependents namespace name
          releases <- liftIO $ withPool pool $ getReleases (package ^. #packageId)
          let latestRelease =  maximumBy (compare `on` version) releases
          latestReleasedependencies <- liftIO $ withPool pool $ getRequirements (latestRelease ^. #releaseId)
          templateEnv <- fromSession session defaultTemplateEnv
          render templateEnv $ Packages.showPackage latestRelease package dependents latestReleasedependencies
    _ -> renderError notFound404

showVersionHandler :: Text -> Text -> Text -> FloraPageM (Html ())
showVersionHandler namespaceText nameText versionText = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  case (validateNamespace namespaceText, validateName nameText, validateVersion versionText) of
    (Just namespace, Just name, Just versionSpec) -> do
      result <- liftIO $ withPool pool $ getPackageByNamespaceAndName namespace name
      case result of
        Nothing -> renderError notFound404
        Just package -> do
          dependents <- liftIO $ withPool pool $ getPackageDependents namespace name
          liftIO (withPool pool $ getReleaseByVersion (package ^. #packageId) versionSpec)
            >>= \case
              Nothing -> renderError notFound404
              Just release -> do
                releaseDependencies <- liftIO $ withPool pool $ getRequirements (release ^. #releaseId)
                templateEnv <- fromSession session defaultTemplateEnv
                render templateEnv $ Packages.showPackage release package dependents releaseDependencies
    _ -> renderError notFound404


validateNamespace :: Text -> Maybe Namespace
validateNamespace txt =
  parseNamespace =<< stripPrefix "@" txt

validateName :: Text -> Maybe PackageName
validateName txt = parsePackageName txt

validateVersion :: Text -> Maybe Version
validateVersion txt = simpleParsec $ T.unpack txt
