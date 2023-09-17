module FloraWeb.Pages.Server.Packages
  ( Routes
  , server
  )
where

import Data.Foldable
import Data.Function
import Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Positive
import Data.Text.Display (display)
import Data.Vector qualified as Vector
import Distribution.Orphans ()
import Distribution.Types.Version (Version)
import Log (object, (.=))
import Log qualified
import Lucid
import Lucid.Orphans ()
import Servant (ServerT)

import Control.Monad.IO.Class
import Flora.Logging
import Flora.Model.Package
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Search qualified as Search
import FloraWeb.Common.Auth
import FloraWeb.Common.Guards
import FloraWeb.Common.Pagination
import FloraWeb.Pages.Routes.Packages
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Error
import FloraWeb.Pages.Templates.Packages qualified as Package
import FloraWeb.Pages.Templates.Pages.Packages qualified as Packages
import FloraWeb.Pages.Templates.Pages.Search qualified as Search
import FloraWeb.Session

server :: ServerT Routes FloraPage
server =
  Routes'
    { index = listPackagesHandler
    , showNamespace = showNamespaceHandler
    , showPackage = showPackageHandler
    , showVersion = showVersionHandler
    , showDependents = showDependentsHandler
    , showVersionDependents = showVersionDependentsHandler
    , showDependencies = showDependenciesHandler
    , showVersionDependencies = showVersionDependenciesHandler
    , showChangelog = showChangelogHandler
    , showVersionChangelog = showVersionChangelogHandler
    , listVersions = listVersionsHandler
    }

listPackagesHandler :: Maybe (Positive Word) -> FloraPage (Html ())
listPackagesHandler pageParam = do
  let pageNumber = pageParam ?: PositiveUnsafe 1
  session <- getSession
  templateDefaults <- fromSession session defaultTemplateEnv
  (count', results) <- Search.listAllPackages (fromPage pageNumber)
  render templateDefaults $ Search.showAllPackages count' pageNumber results

showNamespaceHandler :: Namespace -> Maybe (Positive Word) -> FloraPage (Html ())
showNamespaceHandler namespace pageParam = do
  let pageNumber = pageParam ?: PositiveUnsafe 1
  session <- getSession
  templateDefaults <- fromSession session defaultTemplateEnv
  (count', results) <- Search.listAllPackagesInNamespace namespace (fromPage pageNumber)
  render templateDefaults $
    Search.showAllPackagesInNamespace namespace count' pageNumber results

showPackageHandler :: Namespace -> PackageName -> FloraPage (Html ())
showPackageHandler namespace packageName = do
  showPackageVersion namespace packageName Nothing

showVersionHandler :: Namespace -> PackageName -> Version -> FloraPage (Html ())
showVersionHandler namespace packageName version =
  showPackageVersion namespace packageName (Just version)

showPackageVersion :: Namespace -> PackageName -> Maybe Version -> FloraPage (Html ())
showPackageVersion namespace packageName mversion = do
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  releases <- Query.getReleases (package.packageId)
  liftIO $ putStrLn $ "Number of releases: " <> show (length releases)
  let latestRelease =
        releases
          & Vector.filter (\r -> not (fromMaybe False r.deprecated))
          & maximumBy (compare `on` (.version))
      version = fromMaybe latestRelease.version mversion
  release <- guardThatReleaseExists package.packageId version $ const web404
  numberOfReleases <- Query.getNumberOfReleases package.packageId
  dependents <- Query.getPackageDependents namespace packageName
  releaseDependencies <- Query.getRequirements release.releaseId
  categories <- Query.getPackageCategories (package.packageId)
  numberOfDependents <- Query.getNumberOfPackageDependents namespace packageName
  numberOfDependencies <- Query.getNumberOfPackageRequirements release.releaseId

  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = release.synopsis
          , indexPage = isNothing mversion
          }

  Log.logInfo "displaying a package" $
    object
      [ "release"
          .= object
            [ "id" .= release.releaseId
            , "version" .= display release.version
            ]
      , "dependencies"
          .= object
            [ "count" .= numberOfDependencies
            ]
      , "dependents"
          .= object
            [ "count" .= numberOfDependents
            ]
      , "package" .= (display namespace <> "/" <> display packageName)
      ]

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

showDependentsHandler :: Namespace -> PackageName -> Maybe (Positive Word) -> FloraPage (Html ())
showDependentsHandler namespace packageName mPage = do
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  releases <- Query.getAllReleases (package.packageId)
  let latestRelease = maximumBy (compare `on` (.version)) releases
  showVersionDependentsHandler namespace packageName latestRelease.version mPage

showVersionDependentsHandler :: Namespace -> PackageName -> Version -> Maybe (Positive Word) -> FloraPage (Html ())
showVersionDependentsHandler namespace packageName version Nothing = showVersionDependentsHandler namespace packageName version (Just $ PositiveUnsafe 1)
showVersionDependentsHandler namespace packageName version (Just pageNumber) = do
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  _ <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = "Dependents of " <> display namespace <> display packageName
          }
  results <- Query.getAllPackageDependentsWithLatestVersion namespace packageName (fromPage pageNumber)
  totalDependents <- Query.getNumberOfPackageDependents namespace packageName
  render templateEnv $
    Package.showDependents
      namespace
      packageName
      version
      totalDependents
      results
      pageNumber

showDependenciesHandler :: Namespace -> PackageName -> FloraPage (Html ())
showDependenciesHandler namespace packageName = do
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  releases <- Query.getAllReleases (package.packageId)
  let latestRelease = maximumBy (compare `on` (.version)) releases
  showVersionDependenciesHandler namespace packageName latestRelease.version

showVersionDependenciesHandler :: Namespace -> PackageName -> Version -> FloraPage (Html ())
showVersionDependenciesHandler namespace packageName version = do
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  release <- guardThatReleaseExists package.packageId version $ const web404
  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = "Dependencies of " <> display namespace <> display packageName
          }
  (releaseDependencies, duration) <-
    timeAction $
      Query.getAllRequirements (release.releaseId)

  Log.logInfo "Retrieving all dependencies of the latest release of a package" $
    object
      [ "duration" .= duration
      , "package" .= (display namespace <> "/" <> display packageName)
      , "release_id" .= release.releaseId
      , "component_count" .= Map.size releaseDependencies
      , "dependencies_count" .= Map.foldl' (\acc ds -> acc + Vector.length ds) 0 releaseDependencies
      ]

  render templateEnv $
    Package.showDependencies namespace packageName version releaseDependencies

showChangelogHandler :: Namespace -> PackageName -> FloraPage (Html ())
showChangelogHandler namespace packageName = do
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  releases <- Query.getAllReleases (package.packageId)
  let latestRelease = maximumBy (compare `on` (.version)) releases
  showVersionChangelogHandler namespace packageName (latestRelease.version)

showVersionChangelogHandler :: Namespace -> PackageName -> Version -> FloraPage (Html ())
showVersionChangelogHandler namespace packageName version = do
  Log.logInfo_ $ display namespace
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  release <- guardThatReleaseExists package.packageId version $ const web404
  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = "Changelog of @" <> display namespace <> display packageName
          }

  render templateEnv $ Package.showChangelog namespace packageName version (release.changelog)

listVersionsHandler :: Namespace -> PackageName -> FloraPage (Html ())
listVersionsHandler namespace packageName = do
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName (\_ _ -> web404)
  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = "Releases of " <> display namespace <> display packageName
          }
  releases <- Query.getAllReleases (package.packageId)
  render templateEnv $ Package.listVersions namespace packageName releases
