{-# LANGUAGE LambdaCase #-}

module FloraWeb.Server.Pages.Packages
  ( Routes
  , server
  )
where

import Control.Monad.Reader
import Data.Foldable
import Data.Function
import Data.Text
import qualified Data.Text as T
import Database.PostgreSQL.Entity.DBT (withPool)
import Distribution.Parsec (simpleParsec)
import Distribution.Types.Version (Version)
import Lucid
import Lucid.Orphans ()
import Network.HTTP.Types.Status
import Optics.Core

import Flora.Environment
import Flora.Model.Package
import qualified Flora.Model.Package.Query as Query
import Flora.Model.Release
import qualified Flora.Model.Release.Query as Query
import qualified Flora.Search as Search
import FloraWeb.Routes.Pages.Packages
import FloraWeb.Server.Auth
import FloraWeb.Session
import FloraWeb.Templates
import FloraWeb.Templates.Error
import qualified FloraWeb.Templates.Packages.Dependencies as PackageDependencies
import qualified FloraWeb.Templates.Packages.Dependents as PackageDependents
import qualified FloraWeb.Templates.Pages.Packages as Packages
import qualified FloraWeb.Templates.Pages.Search as Search
import FloraWeb.Types
import Servant (ServerT)

server :: ServerT Routes FloraPageM
server =
  Routes'
    { index = indexHandler
    , show = showHandler
    , showVersion = showVersionHandler
    , showDependents = showDependentsHandler
    , showDependencies = showDependenciesHandler
    }

indexHandler :: FloraPageM (Html ())
indexHandler = do
  session <- getSession
  templateDefaults <- fromSession session defaultTemplateEnv
  results <- Search.listAllPackages
  render templateDefaults $ Search.showAllPackages results

showHandler :: Text -> Text -> FloraPageM (Html ())
showHandler namespaceText packageNameText = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  templateEnv <- fromSession session defaultTemplateEnv
  case (validateNamespace namespaceText, validateName packageNameText) of
    (Just namespace, Just name) -> do
      result <- liftIO $ withPool pool $ Query.getPackageByNamespaceAndName namespace name
      case result of
        Nothing -> renderError templateEnv notFound404
        Just package -> do
          releases <- liftIO $ withPool pool $ Query.getReleases (package ^. #packageId)
          let latestRelease = maximumBy (compare `on` version) releases
          showPackageVersion namespace name (latestRelease ^. #version)
    _ -> renderError templateEnv notFound404

showVersionHandler :: Text -> Text -> Text -> FloraPageM (Html ())
showVersionHandler namespaceText packageNameText versionText = do
  session <- getSession
  templateEnv <- fromSession session defaultTemplateEnv
  case (validateNamespace namespaceText, validateName packageNameText, validateVersion versionText) of
    (Just namespace, Just name, Just versionSpec) -> showPackageVersion namespace name versionSpec
    _ -> renderError templateEnv notFound404

showPackageVersion :: Namespace -> PackageName -> Version -> FloraPageM (Html ())
showPackageVersion namespace packageName version = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  templateEnv <- fromSession session defaultTemplateEnv
  result <- liftIO $ withPool pool $ Query.getPackageByNamespaceAndName namespace packageName
  case result of
    Nothing -> renderError templateEnv notFound404
    Just package -> do
      dependents <- liftIO $ withPool pool $ Query.getPackageDependents namespace packageName
      liftIO (withPool pool $ Query.getReleaseByVersion (package ^. #packageId) version)
        >>= \case
          Nothing -> renderError templateEnv notFound404
          Just release -> do
            releaseDependencies <- liftIO $ withPool pool $ Query.getRequirements (release ^. #releaseId)
            categories <- liftIO $ withPool pool $ Query.getPackageCategories (package ^. #packageId)
            numberOfDependents <- withPool pool $ Query.getNumberOfPackageDependents namespace packageName
            numberOfDependencies <- withPool pool $ Query.getNumberOfPackageRequirements (release ^. #releaseId)
            render templateEnv $ Packages.showPackage release package dependents numberOfDependents releaseDependencies numberOfDependencies categories

showDependentsHandler :: Text -> Text -> FloraPageM (Html ())
showDependentsHandler namespaceText packageNameText = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  templateEnv <- fromSession session defaultTemplateEnv
  case (validateNamespace namespaceText, validateName packageNameText) of
    (Just namespace, Just packageName) -> do
      result <- liftIO $ withPool pool $ Query.getPackageByNamespaceAndName namespace packageName
      case result of
        Nothing -> renderError templateEnv notFound404
        Just _package -> do
          results <- withPool pool $ Query.getAllPackageDependentsWithLatestVersion namespace packageName
          render templateEnv $
            PackageDependents.showDependents ("Dependents of " <> formatPackage namespace packageName) results
    _ -> renderError templateEnv notFound404

showDependenciesHandler :: Text -> Text -> FloraPageM (Html ())
showDependenciesHandler namespaceText packageNameText = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  templateEnv <- fromSession session defaultTemplateEnv
  case (validateNamespace namespaceText, validateName packageNameText) of
    (Just namespace, Just packageName) -> do
      result <- liftIO $ withPool pool $ Query.getPackageByNamespaceAndName namespace packageName
      case result of
        Nothing -> renderError templateEnv notFound404
        Just package -> do
          releases <- liftIO $ withPool pool $ Query.getReleases (package ^. #packageId)
          let latestRelease = maximumBy (compare `on` version) releases
          latestReleasedependencies <-
            liftIO $ withPool pool $ Query.getAllRequirements (latestRelease ^. #releaseId)
          render templateEnv $
            PackageDependencies.showDependencies ("Dependencies of " <> formatPackage namespace packageName) latestReleasedependencies
    _ -> renderError templateEnv notFound404

validateNamespace :: Text -> Maybe Namespace
validateNamespace txt =
  parseNamespace =<< stripPrefix "@" txt

validateName :: Text -> Maybe PackageName
validateName = parsePackageName

validateVersion :: Text -> Maybe Version
validateVersion txt = simpleParsec $ T.unpack txt
