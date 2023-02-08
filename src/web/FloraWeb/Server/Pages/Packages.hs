module FloraWeb.Server.Pages.Packages
  ( Routes
  , server
  )
where

import Data.Foldable
import Data.Function
import Data.Vector qualified as Vector
import Distribution.Types.Version (Version)
import Log (object, (.=))
import Log qualified
import Lucid
import Lucid.Orphans ()
import Servant (ServerT)

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text.Display (display)
import Distribution.Orphans ()
import Flora.Logging
import Flora.Model.Package
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Search qualified as Search
import FloraWeb.Routes.Pages.Packages
import FloraWeb.Server.Auth
import FloraWeb.Server.Guards
import FloraWeb.Session
import FloraWeb.Templates
import FloraWeb.Templates.Packages qualified as Package
import FloraWeb.Templates.Pages.Packages qualified as Packages
import FloraWeb.Templates.Pages.Search qualified as Search

server :: ServerT Routes FloraPage
server =
  Routes'
    { index = indexHandler
    , show = showHandler
    , showVersion = showVersionHandler
    , showDependents = showDependentsHandler
    , showDependencies = showDependenciesHandler
    , showVersionDependencies = showVersionDependenciesHandler
    , showChangelog = showChangelogHandler
    , showVersionChangelog = showVersionChangelogHandler
    , listVersions = listVersionsHandler
    }

indexHandler :: Maybe Word -> FloraPage (Html ())
indexHandler pageParam = do
  let pageNumber = fromMaybe 1 pageParam
  session <- getSession
  templateDefaults <- fromSession session defaultTemplateEnv
  (count', results) <- Search.listAllPackages pageNumber
  render templateDefaults $! Search.showAllPackages count' pageNumber results

showHandler :: Namespace -> PackageName -> FloraPage (Html ())
showHandler namespace packageName = do
  package <- guardThatPackageExists namespace packageName
  releases <- Query.getAllReleases (package.packageId)
  let latestRelease = maximumBy (compare `on` (.version)) releases
  showPackageVersion namespace packageName (latestRelease.version)

showVersionHandler :: Namespace -> PackageName -> Version -> FloraPage (Html ())
showVersionHandler namespace packageName version =
  showPackageVersion namespace packageName version

showPackageVersion :: Namespace -> PackageName -> Version -> FloraPage (Html ())
showPackageVersion namespace packageName version = do
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName
  release <- guardThatReleaseExists namespace packageName version
  releases <- Query.getReleases (package.packageId)
  numberOfReleases <- Query.getNumberOfReleases package.packageId
  dependents <- Query.getPackageDependents namespace packageName
  releaseDependencies <- Query.getRequirements release.releaseId
  categories <- Query.getPackageCategories (package.packageId)
  numberOfDependents <- Query.getNumberOfPackageDependents namespace packageName
  numberOfDependencies <- Query.getNumberOfPackageRequirements release.releaseId

  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = release.metadata.synopsis
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

showDependentsHandler :: Namespace -> PackageName -> Maybe Word -> FloraPage (Html ())
showDependentsHandler namespace packageName Nothing = showDependentsHandler namespace packageName (Just 1)
showDependentsHandler namespace packageName (Just 0) = showDependentsHandler namespace packageName (Just 1)
showDependentsHandler namespace packageName (Just pageNumber) = do
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  _ <- guardThatPackageExists namespace packageName
  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = "Dependents of " <> display namespace <> display packageName
          }
  results <- Query.getAllPackageDependentsWithLatestVersion namespace packageName pageNumber
  totalDependents <- Query.getNumberOfPackageDependents namespace packageName
  render templateEnv $
    Package.showDependents
      namespace
      packageName
      ("Dependents of " <> display namespace <> "/" <> display packageName)
      totalDependents
      results
      pageNumber

showDependenciesHandler :: Namespace -> PackageName -> FloraPage (Html ())
showDependenciesHandler namespace packageName = do
  package <- guardThatPackageExists namespace packageName
  releases <- Query.getAllReleases (package.packageId)
  let latestRelease = maximumBy (compare `on` (.version)) releases
  showVersionDependenciesHandler namespace packageName latestRelease.version

showVersionDependenciesHandler :: Namespace -> PackageName -> Version -> FloraPage (Html ())
showVersionDependenciesHandler namespace packageName version = do
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  release <- guardThatReleaseExists namespace packageName version
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
      , "dependencies_count" .= Vector.length releaseDependencies
      ]

  render templateEnv $
    Package.showDependencies
      ("Dependencies of " <> display namespace <> "/" <> display packageName)
      releaseDependencies

showChangelogHandler :: Namespace -> PackageName -> FloraPage (Html ())
showChangelogHandler namespace packageName = do
  package <- guardThatPackageExists namespace packageName
  releases <- Query.getAllReleases (package.packageId)
  let latestRelease = maximumBy (compare `on` (.version)) releases
  showVersionChangelogHandler namespace packageName (latestRelease.version)

showVersionChangelogHandler :: Namespace -> PackageName -> Version -> FloraPage (Html ())
showVersionChangelogHandler namespace packageName version = do
  Log.logInfo_ $! display namespace
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  void $! guardThatPackageExists namespace packageName
  release <- guardThatReleaseExists namespace packageName version
  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = "Changelog of @" <> display namespace <> display packageName
          }

  render templateEnv $! Package.showChangelog namespace packageName version (release.changelog)

listVersionsHandler :: Namespace -> PackageName -> FloraPage (Html ())
listVersionsHandler namespace packageName = do
  session <- getSession
  templateEnv' <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName
  let templateEnv =
        templateEnv'
          { title = display namespace <> "/" <> display packageName
          , description = "Releases of " <> display namespace <> display packageName
          }
  releases <- Query.getAllReleases (package.packageId)
  render templateEnv $! Package.listVersions namespace packageName releases
