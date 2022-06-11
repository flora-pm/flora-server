module FloraWeb.Server.Pages.Packages
  ( Routes
  , server
  )
where

import Control.Monad.Reader
import Data.Foldable
import Data.Function
import Database.PostgreSQL.Entity.DBT (withPool)
import Distribution.Types.Version (Version)
import Lucid
import Lucid.Orphans ()
import Servant (ServerT)

import Flora.Environment
import Flora.Model.Package
import Flora.Model.Release
import FloraWeb.Routes.Pages.Packages
import FloraWeb.Server.Auth
import FloraWeb.Server.Guards
import FloraWeb.Session
import FloraWeb.Templates
import FloraWeb.Types
import Optics.Core
import qualified Flora.Model.Package.Query as Query
import qualified Flora.Model.Release.Query as Query
import qualified Flora.Search as Search
import qualified FloraWeb.Templates.Packages.Dependencies as PackageDependencies
import qualified FloraWeb.Templates.Packages.Dependents as PackageDependents
import qualified FloraWeb.Templates.Packages.Versions as PackageVersions
import qualified FloraWeb.Templates.Pages.Packages as Packages
import qualified FloraWeb.Templates.Pages.Search as Search

server :: ServerT Routes FloraPageM
server =
  Routes'
    { index = indexHandler
    , show = showHandler
    , showVersion = showVersionHandler
    , showDependents = showDependentsHandler
    , showDependencies = showDependenciesHandler
    , listVersions = listVersionsHandler
    }

indexHandler :: FloraPageM (Html ())
indexHandler = do
  session <- getSession
  templateDefaults <- fromSession session defaultTemplateEnv
  results <- Search.listAllPackages
  render templateDefaults $ Search.showAllPackages results

showHandler :: Namespace -> PackageName -> FloraPageM (Html ())
showHandler namespace packageName = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  package <- guardThatPackageExists namespace packageName
  releases <- liftIO $ withPool pool $ Query.getReleases (package ^. #packageId)
  let latestRelease = maximumBy (compare `on` version) releases
  showPackageVersion namespace packageName (latestRelease ^. #version)

showVersionHandler :: Namespace -> PackageName -> Version -> FloraPageM (Html ())
showVersionHandler namespace packageName version =
  showPackageVersion namespace packageName version

showPackageVersion :: Namespace -> PackageName -> Version -> FloraPageM (Html ())
showPackageVersion namespace packageName version = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  templateEnv <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName
  release <- guardThatReleaseExists namespace packageName version
  releases <- liftIO $ withPool pool $ Query.getReleases (package ^. #packageId)
  numberOfReleases <- liftIO $ withPool pool $ Query.getNumberOfReleases (package ^. #packageId)
  dependents <- liftIO $ withPool pool $ Query.getPackageDependents namespace packageName
  releaseDependencies <- liftIO $ withPool pool $ Query.getRequirements (release ^. #releaseId)
  categories <- liftIO $ withPool pool $ Query.getPackageCategories (package ^. #packageId)
  numberOfDependents <- withPool pool $ Query.getNumberOfPackageDependents namespace packageName
  numberOfDependencies <- withPool pool $ Query.getNumberOfPackageRequirements (release ^. #releaseId)
  render templateEnv $
    Packages.showPackage
      release
      releases
      numberOfReleases
      package
      dependents
      numberOfDependents
      releaseDependencies
      numberOfDependencies
      categories

showDependentsHandler :: Namespace -> PackageName -> FloraPageM (Html ())
showDependentsHandler namespace packageName = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  templateEnv <- fromSession session defaultTemplateEnv
  _ <- guardThatPackageExists namespace packageName
  results <- withPool pool $ Query.getAllPackageDependentsWithLatestVersion namespace packageName
  render templateEnv $
    PackageDependents.showDependents
      ("Dependents of " <> formatPackage namespace packageName)
      results

showDependenciesHandler :: Namespace -> PackageName -> FloraPageM (Html ())
showDependenciesHandler namespace packageName = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  templateEnv <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName
  releases <- liftIO $ withPool pool $ Query.getReleases (package ^. #packageId)
  let latestRelease = maximumBy (compare `on` version) releases
  latestReleasedependencies <-
    liftIO $ withPool pool $ Query.getAllRequirements (latestRelease ^. #releaseId)
  render templateEnv $
    PackageDependencies.showDependencies
      ("Dependencies of " <> formatPackage namespace packageName)
      latestReleasedependencies

listVersionsHandler :: Namespace -> PackageName -> FloraPageM (Html ())
listVersionsHandler namespace packageName = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  templateEnv <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName
  releases <- liftIO $ withPool pool $ Query.getReleases (package ^. #packageId)
  render templateEnv $ PackageVersions.listVersions namespace packageName releases
