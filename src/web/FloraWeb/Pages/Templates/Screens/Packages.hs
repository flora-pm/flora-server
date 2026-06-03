module FloraWeb.Pages.Templates.Screens.Packages where

import Control.Monad
import Control.Monad.Extra (whenJust)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Display
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Algorithms.Intro qualified as MVector
import Distribution.Version
import Lucid
import Servant (toUrlPiece)

import Data.Positive
import Flora.Model.Category.Types (Category (..))
import Flora.Model.Package.Types
import Flora.Model.PackageGroup.Types
import Flora.Model.PackageUploader.Types
import Flora.Model.Release.Types (Release (..))
import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Components.Utils
import FloraWeb.Links qualified as Links
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
    presentationHeader
      numberOfReleases
      latestRelease
      numberOfDependencies
      numberOfDependents
      namespace
      name
      latestRelease.synopsis
      groups
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

presentationHeader :: Word -> Release -> Word -> Word -> Namespace -> PackageName -> Text -> Vector PackageGroupName -> FloraHTML
presentationHeader numberOfReleases release numberOfDependencies numberOfDependents namespace name synopsis groups =
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

          a_ [class_ "tab", href_ (Links.versionsPage namespace name)] $ do
            Icons.history
            (toHtml $ display numberOfReleases <> " Versions") -- TODO: display 'Version' when only one
          a_ [class_ "tab", href_ ("/" <> (toUrlPiece $ Links.packageVersionChangelog namespace name release.version))] $ do
            Icons.logs
            "Changelog"

          a_ [class_ "tab", href_ (Links.dependenciesPage namespace name release.version)] $ do
            Icons.folderTree
            (toHtml $ display numberOfDependencies <> " Dependencies") -- TODO: Display 'Dependency' when only one
          a_ [class_ "tab", href_ (Links.dependentsPage namespace name (PositiveUnsafe 1))] $ do
            Icons.packageSearch
            (toHtml $ display numberOfDependents <> " Dependents") -- TODO: Display 'Dependent' when only one
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

            a_ [class_ "dropdown-item", href_ (Links.versionsPage namespace name)] $ do
              Icons.history
              (toHtml $ display numberOfReleases <> " Versions") -- TODO: display 'Version' when only one
            a_ [class_ "dropdown-item", href_ (toUrlPiece $ Links.packageVersionChangelog namespace name release.version)] $ do
              Icons.logs
              "Changelog"

            a_ [class_ "dropdown-item", href_ (Links.dependenciesPage namespace name release.version)] $ do
              Icons.folderTree
              (toHtml $ display numberOfDependencies <> " Dependencies") -- TODO: Display 'Dependency' when only one
            a_ [class_ "dropdown-item", href_ (Links.dependentsPage namespace name (PositiveUnsafe 1))] $ do
              Icons.packageSearch
              (toHtml $ display numberOfDependents <> " Dependents") -- TODO: Display 'Dependent' when only one

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
        -- TODO: Split into its own function
        section_ [class_ "flow flow--small"] $ do
          h3_ [class_ "title-section"] "Metadata"
          ul_ [class_ "flow flow--tiny", role_ "list"] $ do
            li_ [class_ "cluster cluster--tiny cluster--nowrap cluster--stretch text-break"] $ do
              span_ [class_ "sr-only"] "Last updated"
              span_ [class_ "color-quaternary"] Icons.cloudUpload
              span_ $ do
                time_ [datetime_ "todo", title_ "todo"] "todo last updated" -- TODO: Display last revision or upload (like on packageCard)
                whenJust mUploader $ \uploader -> do
                  -- TODO: is just me or uploader is never shown? (on dev dataset at least)
                  "by"
                  (toHtml uploader.username)
            li_ [class_ "cluster cluster--tiny cluster--nowrap cluster--stretch text-break"] $ do
              span_ [class_ "sr-only"] "License"
              span_ [class_ "color-quaternary"] Icons.scale
              toHtml license
            -- TODO: Display only when there are categories listed
            li_ [class_ "cluster cluster--tiny cluster--nowrap cluster--stretch text-break"] $ do
              span_ [class_ "sr-only"] "Categories"
              span_ [class_ "color-quaternary"] Icons.folder
              span_ $
                displayCategories categories
            li_ [class_ "cluster cluster--tiny cluster--nowrap cluster--stretch text-break"] $ do
              span_ [class_ "color-quaternary"] Icons.users
              p_ [class_ "text-break"] $ do
                "Maintained by: "
                (toHtml maintainer)
            whenJust mLotteryFactor $ \lotteryFactor ->
              li_ [class_ "cluster cluster--tiny cluster--nowrap cluster--stretch text-break"] $ do
                span_ [class_ "color-quaternary"] Icons.shieldUser
                displayLotteryFactor namespace packageName lotteryFactor
        -- TODO: Split into its own function
        section_ [class_ "flow flow--small"] $ do
          h3_ [class_ "title-section"] "Links"
          displayLinks namespace packageName packageIndexURL latestRelease
        -- TODO: Split into its own function
        section_ [class_ "flow flow--small"] $ do
          h3_ [class_ "title-section"] "Installation"
          case deprecationInfo of
            Just inFavourOf -> displayPackageDeprecation inFavourOf
            Nothing ->
              if fromMaybe False deprecated
                then displayReleaseDeprecation (getLatestViableRelease namespace packageName packageReleases)
                else displayInstructions namespace packageName latestRelease
        -- TODO: Split into its own function
        -- TODO: Display when there are tested compilers listed
        section_ [class_ "flow flow--small"] $ do
          h3_ [class_ "title-section"] "Tested Compilers"
          displayTestedWith latestRelease.testedWith
        -- TODO: Make a "Build Targets" section
        -- TODO: Display only when there are package flags
        section_ [class_ "flow flow--small"] $ do
          h3_ [class_ "title-section"] "Package Flags"
          displayPackageFlags flags
      section_ [class_ "flow"] $ do
        h3_ [class_ "title-section"] "Readme"
        -- TODO: Display a fallback message when there is no readme (in dev, the "renderHaddock release.description" is not showing anything (on /@mlabs / plutarch-ledger-api for example))
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

renderCategory :: Category -> FloraHTML
renderCategory Category{name, slug} = do
  let resource = "/categories/" <> slug
  a_ [href_ resource] (toHtml name)
  "," -- TODO: Not display comma after last category

displayCategories :: Vector Category -> FloraHTML
displayCategories categories = foldMap renderCategory categories

displayMaintainer
  :: Namespace
  -> PackageName
  -> Maybe Int
  -> Maybe PackageUploader
  -> Text
  -> FloraHTML
displayMaintainer namespace packageName mLotteryFactor mUploader maintainerInfo =
  p_ [class_ "maintainer-info"] (toHtml maintainerInfo)

displayLotteryFactor
  :: Namespace
  -> PackageName
  -> Int
  -> FloraHTML
displayLotteryFactor namespace packageName lotteryFactor = do
  span_ $ toHtml ("Lottery factor: " <> display lotteryFactor)
  button_
    [ class_ "btn btn--tiny btn--secondary"
    , ariaLabel_ "Learn new feature"
    , interestfor_ "lottery-factor-learn"
    , popovertarget_ "lottery-factor-learn"
    ]
    $ do
      Icons.info
      " New"
  div_
    [ id_ "lottery-factor-learn"
    , class_ "tooltip flow flow--small text-center"
    , popover_ "auto"
    , role_ "tooltip"
    , class_ "revised-date"
    ]
    $ p_
    $ toHtml ("The number of people with uploader permission on " <> formatPackage namespace packageName <> " who have released something to " <> display namespace <> " in the last 2 years (i.e. the number of people likely able to release critical fixes in a timely manner)")

-- displayMaintainer
--   :: Namespace
--   -> PackageName
--   -> Maybe Int
--   -> Maybe PackageUploader
--   -> Text
--   -> FloraHTML
-- displayMaintainer namespace packageName mLotteryFactor mUploader maintainerInfo =
--   li_ [class_ ""] $ do
--     h3_ [class_ "package-body-section"] "Maintainer"
--     div_ [] $ do
--       p_ [class_ "maintainer-info"] (toHtml maintainerInfo)
--       whenJust mLotteryFactor $ \lotteryFactor ->
--         p_ [] $ displayLotteryFactor namespace packageName lotteryFactor
--       whenJust mUploader $ \uploader -> p_ [] $ displayUploader uploader.username

getHomepage :: Release -> Text
getHomepage release =
  case release.homepage of
    Just page -> page
    Nothing ->
      if Vector.null release.sourceRepos
        then "⚠  No homepage provided"
        else Vector.head release.sourceRepos

displaySourceRepos :: Vector Text -> FloraHTML
displaySourceRepos x
  | Vector.null x = toHtml @Text "No source repository"
  | otherwise = a_ [href_ (Vector.head x)] "Source repository"

displayLinks :: Namespace -> PackageName -> Text -> Release -> FloraHTML
displayLinks namespace packageName packageIndexURL release = do
  ul_ [class_ "flow flow--tiny", role_ "list"] $ do
    when (release.homepage /= Just "") $
      li_ [class_ "cluster cluster--tiny cluster--nowrap cluster--stretch"] $ do
        span_ [class_ "color-quaternary"] Icons.house
        a_ [href_ (getHomepage release)] "Homepage"
    li_ [class_ "cluster cluster--tiny cluster--nowrap cluster--stretch"] $ do
      span_ [class_ "color-quaternary"] Icons.bookText
      a_ [href_ (packageIndexURL <> "/package/" <> display packageName <> "-" <> display release.version)] "Documentation"
    li_ [class_ "cluster cluster--tiny cluster--nowrap cluster--stretch"] $ do
      span_ [class_ "color-quaternary"] Icons.code
      displaySourceRepos release.sourceRepos

displayTestedWith :: Vector Version -> FloraHTML
displayTestedWith compilersVersions'
  | Vector.null compilersVersions' = mempty
  | otherwise = do
      let compilersVersions = Vector.reverse $ Vector.modify MVector.sort compilersVersions'
      ol_ [class_ "cluster cluster--small", role_ "list"] $
        Vector.forM_
          compilersVersions
          (li_ [class_ "badge tabular-nums"] . toHtml @Text . display)
