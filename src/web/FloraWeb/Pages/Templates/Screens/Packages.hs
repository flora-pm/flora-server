module FloraWeb.Pages.Templates.Screens.Packages where

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Algorithms.Intro qualified as MVector
import Distribution.Version
import Lucid

import Flora.Model.Category.Types (Category (..))
import Flora.Model.Package.Types
import Flora.Model.PackageGroup.Types
import Flora.Model.PackageUploader.Types
import Flora.Model.Release.Types (Release (..))
import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Components.Utils
import FloraWeb.Pages.Templates.Packages
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
  -> Vector DependencyVersionRequirement
  -> Word
  -> Vector Category
  -> Vector PackageGroupName
  -> Maybe (Vector Text)
  -> Maybe PackageUploader
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
  categories
  groups
  activeMaintainers
  mUploader = do
    presentationHeader latestRelease namespace name latestRelease.synopsis groups
    div_ [class_ "wrapper inset-large"] $ do
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
        (fmap Vector.length activeMaintainers)
        mUploader

presentationHeader :: Release -> Namespace -> PackageName -> Text -> Vector PackageGroupName -> FloraHTML
presentationHeader release namespace name synopsis groups =
  header_ [class_ "pageHead"] $ do
    div_ [class_ "wrapper flow flow--large"] $ do
      div_ [class_ "aside gap--large"] $ do
        div_ [class_ "flow"] $ do
          h1_ [class_ "pageHead-title tracking-tight"] $ do
            span_ [class_ "prefix"] $ do
              -- TODO: Fix link
              a_ [href_ (display namespace)] (toHtml $ display namespace)
              (toHtmlRaw ("&ThinSpace;/&ThinSpace;" :: Text))
            toHtml name
          p_ [class_ "pageHead-subtitle text-break"] (toHtml synopsis)
        div_ [class_ "flow flow--small self-center"] $ do
          div_ [class_ "cluster cluster--small items-end"] $ do
            -- TODO: Display only on latest release page (not when no version specified)
            -- span_ [class_ "badge badge--big badge--green"] $ do
            --   Icons.check
            --   "Latest"
            -- TODO: Display on deprecated releases
            -- span_ [class_ "badge badge--big badge--danger"] $ do
            --   Icons.trash
            --   "Deprecated"
            span_ [class_ "title-2 text-right leading-thin"] $ toHtml release.version
      div_ [class_ "pageHead-tip"] $ do
        -- TODO: Split tabs in a separate function
        -- TODO: Display actual link, labels, and current attribute/class
        nav_ [class_ "tabs", id_ "subsections", ariaLabel_ "Package sections"] $ do
          a_ [class_ "tab", href_ "/", ariaCurrent_ "page"] $ do
            Icons.bookOpenText
            "About"
          a_ [class_ "tab", href_ "/"] $ do
            Icons.history
            "49 Versions" -- TODO: Display real number
          a_ [class_ "tab", href_ "/"] $ do
            Icons.logs
            "Changelog"
          a_ [class_ "tab", href_ "/"] $ do
            Icons.folderTree
            "1 Dependency" -- TODO: Display real number + dependency/dependencies
          a_ [class_ "tab", href_ "/"] $ do
            Icons.packageSearch
            "5 Dependents" -- TODO: Display real number + dependent/dependent
        div_ [class_ "tabs-mobile", id_ "subsectionsMobile"] $ do
          -- TODO: Display current section in aria-label attribute
          button_ [class_ "tabs-mobileBtn btn btn--secondary", ariaLabel_ ("Switch section (Current: " <> "About" <> ")"), popovertarget_ "subsectionsMobile-menu"] $ do
            Icons.bookOpenText
            div_ [class_ "flex-grow"] $ do
              div_ [class_ "prefix"] $ "Current section"
              div_ $ "About" -- TODO: Display current section label
            Icons.chevronUpDown
          nav_ [class_ "dropdown dropdown--full", id_ "subsectionsMobile-menu", ariaLabel_ "Package sections", popover_ ""] $ do
            a_ [class_ "dropdown-item dropdown-item--current", href_ "/", ariaCurrent_ "page"] $ do
              Icons.bookOpenText
              "About"
            a_ [class_ "dropdown-item", href_ "/"] $ do
              Icons.history
              "49 Versions" -- TODO: Display real number
            a_ [class_ "dropdown-item", href_ "/"] $ do
              Icons.logs
              "Changelog"
            a_ [class_ "dropdown-item", href_ "/"] $ do
              Icons.folderTree
              "1 Dependency" -- TODO: Display real number + dependency/dependencies
            a_ [class_ "dropdown-item", href_ "/"] $ do
              Icons.packageSearch
              "5 Dependents" -- TODO: Display real number + dependent/dependent

packageBody
  :: Package
  -> Text
  -> Release
  -> Vector Release
  -> Word
  -> Vector DependencyVersionRequirement
  -> Word
  -> Vector Package
  -> Word
  -> Vector Category
  -> Maybe Int
  -> Maybe PackageUploader
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
  categories
  mLotteryFactor
  mUploader = do
    h2_ [class_ "sr-only"] "About"
    div_ [class_ "package-about aside aside--reverse aside--start"] $ do
      div_ [class_ "package-details"] $ do
        displayCategories categories
        displayLicense license
        displayMaintainer namespace packageName mLotteryFactor mUploader maintainer
        displayLinks namespace packageName packageIndexURL latestRelease
        displayVersions namespace packageName packageReleases numberOfReleases
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
      section_ [class_ "flow"] $ do
        h3_ [class_ "title-section"] "Readme"
        div_ [class_ "prose"] $ displayReadme latestRelease

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
