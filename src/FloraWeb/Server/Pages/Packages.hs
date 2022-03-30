{-# LANGUAGE LambdaCase #-}

module FloraWeb.Server.Pages.Packages
  ( Routes
  , server
  ) where

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
import FloraWeb.Routes.Pages.Packages
import FloraWeb.Server.Auth
import FloraWeb.Server.Util (redirect)
import FloraWeb.Session
import FloraWeb.Templates
import FloraWeb.Templates.Error
import qualified FloraWeb.Templates.Pages.Packages as Packages
import FloraWeb.Types
import Servant (Header, Headers, NoContent, ServerT)

server :: ServerT Routes FloraPageM
server = Routes'
  { index = indexHandler
  , show = showHandler
  , showVersion = showVersionHandler
  }

indexHandler :: FloraPageM (Headers '[Header "Location" Text] NoContent)
indexHandler = pure $ redirect "/"

showHandler :: Text -> Text -> FloraPageM (Html ())
showHandler namespaceText nameText = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  templateEnv <- fromSession session defaultTemplateEnv
  case (validateNamespace namespaceText, validateName nameText) of
    (Just namespace, Just name) -> do
      result <- liftIO $ withPool pool $ Query.getPackageByNamespaceAndName namespace name
      case result of
        Nothing -> renderError templateEnv notFound404
        Just package -> do
          dependents <- liftIO $ withPool pool $ Query.getPackageDependents namespace name
          releases <- liftIO $ withPool pool $ Query.getReleases (package ^. #namespace, package ^. #name)
          let latestRelease =  maximumBy (compare `on` version) releases
          latestReleasedependencies <- liftIO $ withPool pool $ Query.getRequirements (latestRelease ^. #releaseId)
          categories <- liftIO $ withPool pool $ Query.getPackageCategories (package ^. #namespace) (package ^. #name)
          render templateEnv $ Packages.showPackage latestRelease package dependents latestReleasedependencies categories
    _ -> renderError templateEnv notFound404

showVersionHandler :: Text -> Text -> Text -> FloraPageM (Html ())
showVersionHandler namespaceText nameText versionText = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  templateEnv <- fromSession session defaultTemplateEnv
  case (validateNamespace namespaceText, validateName nameText, validateVersion versionText) of
    (Just namespace, Just name, Just versionSpec) -> do
      result <- liftIO $ withPool pool $ Query.getPackageByNamespaceAndName namespace name
      case result of
        Nothing -> renderError templateEnv notFound404
        Just package -> do
          dependents <- liftIO $ withPool pool $ Query.getPackageDependents namespace name
          liftIO (withPool pool $ Query.getReleaseByVersion (package ^. #namespace, package ^. #name) versionSpec)
            >>= \case
              Nothing -> renderError templateEnv notFound404
              Just release -> do
                releaseDependencies <- liftIO $ withPool pool $ Query.getRequirements (release ^. #releaseId)
                categories <- liftIO $
                  withPool pool $ Query.getPackageCategories (package ^. #namespace) (package ^. #name)
                render templateEnv $ Packages.showPackage release package dependents releaseDependencies categories
    _ -> renderError templateEnv notFound404

validateNamespace :: Text -> Maybe Namespace
validateNamespace txt =
  parseNamespace =<< stripPrefix "@" txt

validateName :: Text -> Maybe PackageName
validateName = parsePackageName

validateVersion :: Text -> Maybe Version
validateVersion txt = simpleParsec $ T.unpack txt
