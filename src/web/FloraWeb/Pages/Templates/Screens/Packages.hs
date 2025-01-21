module FloraWeb.Pages.Templates.Screens.Packages where

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Algorithms.Intro qualified as MVector
import Distribution.Version
import Lucid

import Flora.Model.Category.Types (Category (..))
import Flora.Model.Package.Types
import Flora.Model.Release.Types (Release (..))
import FloraWeb.Components.Icons (chevronRightOutline)
import FloraWeb.Pages.Templates.Packages
  ( displayCategories
  , displayDependencies
  , displayDependents
  , displayInstructions
  , displayLicense
  , displayLinks
  , displayMaintainer
  , displayNamespace
  , displayPackageDeprecation
  , displayPackageFlags
  , displayReadme
  , displayReleaseDeprecation
  , displayTestedWith
  , displayVersions
  )
import FloraWeb.Pages.Templates.Types (FloraHTML)
import Lucid.Orphans ()

showPackage
  :: Release
  -> Vector Release
  -> Word
  -> Package
  -> Text
  -> Vector Package
  -> Word
  -> Vector (Namespace, PackageName, Text)
  -> Word
  -> Vector Category
  -> FloraHTML
showPackage
  latestRelease
  packageReleases
  numberOfReleases
  package@Package{namespace, name}
  packageIndexURL
  dependents
  numberOfDependents
  dependencies
  numberOfDependencies
  categories =
    div_ [class_ "container"] $ do
      presentationHeader latestRelease namespace name latestRelease.synopsis
      packageBody
        package
        packageIndexURL
        latestRelease
        packageReleases
        numberOfReleases
        dependencies
        numberOfDependencies
        dependents
        numberOfDependents
        categories

presentationHeader :: Release -> Namespace -> PackageName -> Text -> FloraHTML
presentationHeader release namespace name synopsis =
  div_ [class_ "divider"] $ do
    div_ [class_ "page-title"] $ do
      h1_ [class_ "package-title"] $ do
        span_ [class_ "headline"] $ do
          displayNamespace namespace
          chevronRightOutline
          toHtml name
        let versionClass = "version" <> if Just True == release.deprecated then " release-deprecated" else ""
        span_ [class_ versionClass] $ toHtml release.version
    div_ [class_ "synopsis"] $
      p_ [class_ ""] (toHtml synopsis)

packageBody
  :: Package
  -> Text
  -> Release
  -> Vector Release
  -> Word
  -> Vector (Namespace, PackageName, Text)
  -> Word
  -> Vector Package
  -> Word
  -> Vector Category
  -> FloraHTML
packageBody
  Package{namespace, name = packageName, deprecationInfo}
  packageIndexURL
  latestRelease@Release{flags, deprecated, license, maintainer, version}
  packageReleases
  numberOfReleases
  dependencies
  numberOfDependencies
  dependents
  numberOfDependents
  categories =
    div_ [class_ "package-body"] $ do
      div_ [class_ "package-left-column"] $ ul_ [class_ "package-left-rows"] $ do
        displayCategories categories
        displayLicense license
        displayMaintainer maintainer
        displayLinks namespace packageName packageIndexURL latestRelease
        displayVersions namespace packageName packageReleases numberOfReleases
      div_ [class_ "package-right-column"] $ ul_ [class_ "package-right-rows"] $ do
        case deprecationInfo of
          Just inFavourOf -> displayPackageDeprecation inFavourOf
          Nothing ->
            if fromMaybe False deprecated
              then displayReleaseDeprecation (getLatestViableRelease namespace packageName packageReleases)
              else displayInstructions namespace packageName latestRelease
        displayTestedWith latestRelease.testedWith
        displayDependencies (namespace, packageName, version) numberOfDependencies dependencies
        displayDependents (namespace, packageName) numberOfDependents dependents
        displayPackageFlags flags
      div_ [class_ "release-readme-column"] $ div_ [class_ "release-readme"] $ displayReadme latestRelease

getLatestViableRelease
  :: Namespace
  -> PackageName
  -> Vector Release
  -> Maybe (Namespace, PackageName, Version)
getLatestViableRelease namespace packageName releases =
  releases
    & Vector.filter (\r -> not (fromMaybe False r.deprecated))
    & Vector.modify (MVector.sortBy (\r1 r2 -> compare r2.version r1.version))
    & Vector.uncons
    & \case
      Just (x, _) -> Just (namespace, packageName, x.version)
      Nothing -> Nothing
