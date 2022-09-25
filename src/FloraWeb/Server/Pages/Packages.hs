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

import Data.Maybe (fromMaybe)
import Data.Text.Display (display)
import Distribution.Orphans ()
import Flora.Model.Package
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Search qualified as Search
import FloraWeb.Routes.Pages.Packages
import FloraWeb.Server.Auth
import FloraWeb.Server.Guards
import FloraWeb.Server.Logging
import FloraWeb.Session
import FloraWeb.Templates
import FloraWeb.Templates.Packages.Dependencies qualified as PackageDependencies
import FloraWeb.Templates.Packages.Dependents qualified as PackageDependents
import FloraWeb.Templates.Packages.Versions qualified as PackageVersions
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
    , listVersions = listVersionsHandler
    }

indexHandler :: Maybe Word -> FloraPage (Html ())
indexHandler pageParam = do
  let pageNumber = fromMaybe 1 pageParam
  session <- getSession
  templateDefaults <- fromSession session defaultTemplateEnv
  (count', results) <- Search.listAllPackages pageNumber
  render templateDefaults $ Search.showAllPackages count' pageNumber results

showHandler :: Namespace -> PackageName -> FloraPage (Html ())
showHandler namespace packageName = do
  package <- guardThatPackageExists namespace packageName
  releases <- Query.getAllReleases (package.packageId)
  let latestRelease = maximumBy (compare `on` version) releases
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
          { title = display namespace <> "/" <> display packageName <> " on Flora"
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

showDependentsHandler :: Namespace -> PackageName -> FloraPage (Html ())
showDependentsHandler namespace packageName = do
  session <- getSession
  templateEnv <- fromSession session defaultTemplateEnv
  _ <- guardThatPackageExists namespace packageName
  results <- Query.getAllPackageDependentsWithLatestVersion namespace packageName
  render templateEnv $
    PackageDependents.showDependents
      ("Dependents of " <> display namespace <> "/" <> display packageName)
      results

showDependenciesHandler :: Namespace -> PackageName -> FloraPage (Html ())
showDependenciesHandler namespace packageName = do
  Log.logInfo_ $ display $ Prelude.show namespace
  session <- getSession
  templateEnv <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName
  releases <- Query.getAllReleases (package.packageId)
  let latestRelease = maximumBy (compare `on` version) releases
  (latestReleasedependencies, duration) <-
    timeAction $
      Query.getAllRequirements (latestRelease.releaseId)

  Log.logInfo "Retrieving all dependencies of the latest release of a package" $
    object
      [ "duration" .= duration
      , "package" .= (display namespace <> "/" <> display packageName)
      , "release_id" .= latestRelease.releaseId
      , "dependencies_count" .= Vector.length latestReleasedependencies
      ]

  render templateEnv $
    PackageDependencies.showDependencies
      ("Dependencies of " <> display namespace <> "/" <> display packageName)
      latestReleasedependencies

listVersionsHandler :: Namespace -> PackageName -> FloraPage (Html ())
listVersionsHandler namespace packageName = do
  session <- getSession
  templateEnv <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName
  releases <- Query.getAllReleases (package.packageId)
  render templateEnv $ PackageVersions.listVersions namespace packageName releases
