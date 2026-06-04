module FloraWeb.Pages.Templates.Screens.Packages
  ( showPackage
  , packageBody
  ) where

import Control.Monad
import Control.Monad.Extra (whenJust)
import Data.Function ((&))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text.Display
import Data.Time qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Algorithms.Intro qualified as MVector
import Distribution.Version
import Lucid

import Flora.Model.Category.Types (Category (..))
import Flora.Model.Package.Types
import Flora.Model.PackageGroup.Types
import Flora.Model.PackageUploader.Types
import Flora.Model.Release.Types (Release (..), ReleaseFlags (..))
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
  -> Word
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
  numberOfDependents
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
      "about"
    div_ [class_ "wrapper inset-large"] $ do
      packageBody
        package
        packageIndexURL
        latestRelease
        packageReleases
        categories
        (fmap Vector.length activeMaintainers)
        mUploader

packageBody
  :: Package
  -> Text
  -> Release
  -> Vector Release
  -> Vector Category
  -> Maybe Int
  -> Maybe PackageUploader
  -> FloraHTML
packageBody
  Package{namespace, name = packageName, deprecationInfo}
  packageIndexURL
  latestRelease@Release{flags, deprecated, license, maintainer, revisedAt, uploadedAt}
  packageReleases
  categories
  mLotteryFactor
  mUploader = do
    h2_ [class_ "sr-only"] "About"
    div_ [class_ "package-about aside aside--reverse aside--start"] $ do
      div_ [class_ "package-details"] $ do
        -- TODO: [non-urgent] Split into its own function
        section_ [class_ "flow flow--small"] $ do
          h3_ [class_ "title-section"] "Metadata"
          ul_ [class_ "flow flow--tiny", role_ "list"] $ do
            li_ [class_ "cluster cluster--tiny cluster--nowrap cluster--stretch text-break"] $ do
              span_ [class_ "sr-only"] "Last updated"
              span_ [class_ "color-quaternary"] Icons.cloudUpload
              let mLastUploadedAt = if isJust revisedAt then revisedAt else uploadedAt
              whenJust mLastUploadedAt $ \timestamp -> do
                span_ $ do
                  let timeLabelFull = display (Time.formatTime Time.defaultTimeLocale "%a, %_d %b %Y, %R %EZ" timestamp)
                  let formattedTime = display (Time.formatTime Time.defaultTimeLocale "%_d %b %Y" timestamp)
                  time_ [datetime_ formattedTime, title_ ("Uploaded: " <> timeLabelFull)] (toHtml formattedTime)
                  whenJust mUploader $ \uploader -> do
                    ", by " <> (toHtml uploader.username)
            li_ [class_ "cluster cluster--tiny cluster--nowrap cluster--stretch text-break"] $ do
              span_ [class_ "sr-only"] "License"
              span_ [class_ "color-quaternary"] Icons.scale
              toHtml license
            unless (Vector.null categories) $
              li_ [class_ "cluster cluster--tiny cluster--nowrap cluster--stretch text-break"] $ do
                span_ [class_ "sr-only"] "Categories"
                span_ [class_ "color-quaternary"] Icons.folder
                span_ $
                  displayCategories categories
            li_ [class_ "cluster cluster--tiny cluster--nowrap cluster--stretch text-break"] $ do
              span_ [class_ "color-quaternary"] Icons.users
              p_ [class_ "text-break"] $ do
                "Maintained by: "
                toHtml maintainer
            whenJust mLotteryFactor $ \lotteryFactor ->
              li_ [class_ "cluster cluster--tiny cluster--nowrap cluster--stretch text-break"] $ do
                span_ [class_ "color-quaternary"] Icons.shieldUser
                displayLotteryFactor namespace packageName lotteryFactor
        -- TODO: [non-urgent] Split into its own function
        section_ [class_ "flow flow--small"] $ do
          h3_ [class_ "title-section"] "Links"
          displayLinks namespace packageName packageIndexURL latestRelease
        -- TODO: [non-urgent] Split into its own function
        section_ [class_ "flow flow--small"] $ do
          h3_ [class_ "title-section"] "Installation"
          case deprecationInfo of
            Just inFavourOf -> displayPackageDeprecation inFavourOf
            Nothing ->
              if fromMaybe False deprecated
                then displayReleaseDeprecation (getLatestViableRelease namespace packageName packageReleases)
                else displayInstructions namespace packageName latestRelease
        -- TODO: [non-urgent] Split into its own function
        unless (Vector.null latestRelease.testedWith) $
          section_ [class_ "flow flow--small"] $ do
            h3_ [class_ "title-section"] "Tested Compilers"
            displayTestedWith latestRelease.testedWith
        -- TODO: [non-urgent] Make a "Build Targets" section
        case flags of
          ReleaseFlags f -> unless (Vector.null f) $
            section_ [class_ "flow flow--small"] $ do
              h3_ [class_ "title-section"] "Package Flags"
              displayPackageFlags flags
      section_ [class_ "flow"] $ do
        h3_ [class_ "title-section"] "Readme"
        -- TODO: [non-urgent] Display a fallback message when there is no readme (in dev, the "renderHaddock release.description" is not showing anything (on /@mlabs / plutarch-ledger-api for example))
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
  ", " -- TODO: Not display comma after last category

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
