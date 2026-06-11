module FloraWeb.Pages.Templates.Packages
  ( presentationHeader
  , displayDependencies
  , displayDependents
  , displayInstructions
  , displayLicense
  , displayNamespace
  , displayPackageDeprecation
  , displayPackageFlags
  , displayReadme
  , displayReleaseDeprecation
  , displayReleaseVersion
  , displayVersions
  , listVersions
  , packageListing
  , packageWithExecutableListing
  , showChangelog
  , showDependencies
  , showDependents
  , showPackageSecurityPage
  , packageAdvisoriesListing
  , formatUploadTime
  , seconds
  , minutes
  , hours
  , days
  ) where

import Control.Monad (when)
import Control.Monad.Extra (whenJust)
import Control.Monad.Reader (ask)
import Data.Fixed (Pico, div')
import Data.Foldable (fold, forM_)
import Data.List qualified as List
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Pretty (pretty)
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.BuildType (BuildType (..))
import Distribution.Types.Flag (PackageFlag (..))
import Distribution.Types.Flag qualified as Flag
import Distribution.Types.Version (Version, mkVersion, versionNumbers)
import Lucid
import Servant (toUrlPiece)
import Text.PrettyPrint (Doc, hcat, render)
import Text.PrettyPrint qualified as PP

import Advisories.Model.Affected.Types
import Data.Positive
import Distribution.Orphans ()
import Flora.Environment.Env (FeatureEnv (..))
import Flora.Model.Package.Types
import Flora.Model.PackageGroup.Types
import Flora.Model.Release.Types
import Flora.Model.Requirement
import Flora.Search (SearchAction (..))
import FloraWeb.Components.AdvisoryListItem
import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Components.PackageCard
  ( PackageCardProps (..)
  , packageCard
  )
import FloraWeb.Components.PackageListItem
  ( packageWithExecutableListItem
  , requirementListItem
  )
import FloraWeb.Components.PaginationNav (paginationNav)
import FloraWeb.Components.Pill (customBuildType)
import FloraWeb.Components.Utils
import FloraWeb.Links qualified as Links
import FloraWeb.Pages.Templates (FloraHTML, TemplateEnv (..))
import FloraWeb.Pages.Templates.Haddock (renderHaddock)

data Target
  = Dependents
  | Dependencies
  | Versions
  | Security
  deriving stock (Eq, Ord)

instance Display Target where
  displayBuilder Dependents = "dependents"
  displayBuilder Dependencies = "dependencies"
  displayBuilder Versions = "versions"
  displayBuilder Security = "security"

showDependents
  :: UTCTime
  -> Word
  -> Release
  -> Word
  -> Word
  -> Package
  -> Vector DependencyInfo
  -> Positive Word
  -> Bool
  -> FloraHTML
showDependents now numberOfReleases latestRelease numberOfDependencies numberOfDependents package packagesInfo currentPage latestViableRelease = do
  presentationHeader
    numberOfReleases
    latestRelease
    numberOfDependencies
    numberOfDependents
    package
    mempty
    "dependents"
    False
    latestViableRelease
  section_ [class_ "wrapper inset-large flow", id_ "content"] $ do
    h2_ [class_ "title-2"] "Dependents"
    ul_ [class_ "flow", role_ "list"] $ do
      Vector.forM_
        packagesInfo
        ( \dep -> do
            let link = Links.packageResource dep.namespace dep.name
            let mLastUploadedAt = if isJust dep.revisedAt then dep.revisedAt else dep.uploadedAt
            li_ [] $
              packageCard
                now
                PackageCardProps
                  { link = link
                  , namespace = dep.namespace
                  , name = dep.name
                  , synopsis = dep.latestSynopsis
                  , mVersion = Just (display dep.latestVersion)
                  , mLastUploadedAt = mLastUploadedAt
                  , mLicense = Just dep.latestLicense
                  , exactMatch = False
                  }
        )
    when (numberOfDependents > 30) $
      paginationNav numberOfDependents currentPage (DependentsOf package.namespace package.name Nothing)

showDependencies
  :: UTCTime
  -> Word
  -> Release
  -> Word
  -> Word
  -> Package
  -> ComponentDependencies
  -> Bool
  -> FloraHTML
showDependencies now numberOfReleases latestRelease numberOfDependencies numberOfDependents package componentsInfo isLatestViableRelease = do
  presentationHeader
    numberOfReleases
    latestRelease
    numberOfDependencies
    numberOfDependents
    package
    mempty
    "dependencies"
    True
    isLatestViableRelease
  section_ [class_ "wrapper inset-large flow", id_ "content"] $ do
    h2_ [class_ "title-2"] "Dependencies"
    ul_ [class_ "flow", role_ "list"] $ do
      requirementListItem now componentsInfo

listVersions
  :: Release
  -> UTCTime
  -> Word
  -> Word
  -> Package
  -> Vector Release
  -> Bool
  -> FloraHTML
listVersions latestRelease now numberOfDependencies numberOfDependents package releases isLatestViableRelease = do
  presentationHeader
    (fromIntegral $ Vector.length releases)
    latestRelease
    numberOfDependencies
    numberOfDependents
    package
    mempty
    "versions"
    False
    isLatestViableRelease

  section_ [class_ "wrapper inset-large flow", id_ "content"] $ do
    h2_ [class_ "title-2"] "Version history"
    ul_ [class_ "flow", role_ "list"] $ do
      Vector.forM_
        releases
        (versionListItem now package.namespace package.name)

versionListItem :: UTCTime -> Namespace -> PackageName -> Release -> FloraHTML
versionListItem now namespace packageName release = do
  let uploadedAt = case release.uploadedAt of
        Nothing -> ""
        Just ts -> do
          let timeLabelFull = display (Time.formatTime Time.defaultTimeLocale "%a, %_d %b %Y, %R %EZ" ts)
          li_ [title_ ("Uploaded: " <> timeLabelFull)] $ do
            span_ [class_ "color-tertiary"] Icons.cloudUpload
            span_ [class_ "sr-only"] "Uploaded: "
            time_ [datetime_ timeLabelFull, title_ ("Uploaded: " <> timeLabelFull)] (toHtml $ formatUploadTime ts now)
  let link = "/packages/" <> display namespace <> "/" <> display packageName <> "/" <> display release.version
  li_ $ do
    a_ [href_ link, class_ "entityCard"] $ do
      div_ [class_ "cluster cluster--tiny"] $ do
        span_ [class_ "entityCard-title"] (toHtml release.version)
        whenJust release.deprecated $ \isDeprecated -> do
          when isDeprecated $
            span_ [class_ "badge badge--danger"] $ do
              span_ [class_ "sr-only"] "Version "
              Icons.trash
              "Deprecated"
      -- TODO: [non-urgent] Display on latest non-deprecated release
      -- span_ [class_ "badge badge--green"] $ do
      --   "Latest Release"
      ul_ [class_ "cluster color-secondary text-small", role_ "list"] $ do
        uploadedAt
        case release.revisedAt of
          Nothing -> span_ [] ""
          Just revisionDate -> do
            let timeLabelFull = display (Time.formatTime Time.defaultTimeLocale "%a, %_d %b %Y, %R %EZ" revisionDate)
            li_ [title_ ("Revised: " <> timeLabelFull)] $ do
              span_ [class_ "color-tertiary"] Icons.pen
              span_ [class_ "sr-only"] "Revised: "
              time_ [datetime_ timeLabelFull, title_ ("Revised: " <> timeLabelFull)] (toHtml $ formatUploadTime revisionDate now)
        li_ $ do
          span_ [class_ "color-tertiary"] Icons.scale
          span_ [class_ "sr-only"] "License: "
          toHtml release.license

-- | Render a list of package information
packageListing
  :: UTCTime
  -> Maybe (Vector PackageInfo)
  -- ^ Priority items that are highlighted,
  -- like exact matches for a search
  -> Vector PackageInfo
  -> FloraHTML
packageListing now mExactMatchItems packages =
  ul_ [class_ "flow flow--small", role_ "list"] $ do
    whenJust mExactMatchItems $ \exactMatchItems ->
      forM_ exactMatchItems $ \em -> do
        let link = "/packages/" <> display em.namespace <> "/" <> display em.name
        let mLastUploadedAt = if isJust em.revisedAt then em.revisedAt else em.uploadedAt
        li_ $
          packageCard
            now
            PackageCardProps
              { link = link
              , namespace = em.namespace
              , name = em.name
              , synopsis = em.synopsis
              , mVersion = Just (display em.version)
              , mLastUploadedAt = mLastUploadedAt
              , mLicense = Just em.license
              , exactMatch = True
              }
    Vector.forM_ packages $ \p -> do
      let link = "/packages/" <> display p.namespace <> "/" <> display p.name
      let mLastUploadedAt = if isJust p.revisedAt then p.revisedAt else p.uploadedAt
      li_ $
        packageCard
          now
          PackageCardProps
            { link = link
            , namespace = p.namespace
            , name = p.name
            , synopsis = p.synopsis
            , mVersion = Just (display p.version)
            , mLastUploadedAt = mLastUploadedAt
            , mLicense = Just p.license
            , exactMatch = False
            }

packageWithExecutableListing
  :: Vector PackageInfoWithExecutables
  -> FloraHTML
packageWithExecutableListing packages =
  ul_ [class_ "package-list"] $ do
    Vector.forM_ packages packageWithExecutableListItem

showChangelog
  :: Word
  -> Release
  -> Word
  -> Word
  -> Package
  -> Maybe TextHtml
  -> Bool
  -> FloraHTML
showChangelog numberOfReleases latestRelease numberOfDependencies numberOfDependents package mChangelog latestViableRelease = do
  presentationHeader
    numberOfReleases
    latestRelease
    numberOfDependencies
    numberOfDependents
    package
    mempty
    "changelog"
    True
    latestViableRelease
  section_ [class_ "wrapper inset-large flow", id_ "content"] $ do
    h2_ [class_ "title-2"] "Changelog"
    div_ [class_ "prose"] $ do
      case mChangelog of
        Nothing -> toHtml @Text "This release does not have a Changelog"
        Just changelogText -> toHtmlRaw changelogText

displayReleaseVersion :: Version -> FloraHTML
displayReleaseVersion = toHtml

-- | Display a namespace with a link
displayNamespace :: Namespace -> FloraHTML
displayNamespace namespace =
  a_
    [ class_ "breadcrumb-segment"
    , href_
        ("/packages/" <> display namespace <> "?page=1")
    ]
    (toHtml $ display namespace)

displayLicense :: SPDX.License -> FloraHTML
displayLicense license =
  li_ [class_ ""] $ do
    div_ [class_ "license"] $ h3_ [class_ "package-body-section"] "License"
    p_ [class_ "package-body-section__license"] $ toHtml license

displayReadme :: Release -> FloraHTML
displayReadme release =
  case release.readme of
    Nothing -> renderHaddock release.description
    Just readme -> toHtmlRaw readme

displayVersions :: Namespace -> PackageName -> Vector Release -> Word -> FloraHTML
displayVersions namespace packageName versions numberOfReleases =
  li_ [class_ ""] $ do
    h3_ [class_ "package-body-section versions"] "Versions"
    ul_ [class_ "package-versions"] $ do
      Vector.forM_ versions displayVersion
      if fromIntegral (Vector.length versions) >= numberOfReleases
        then ""
        else showAll Versions Nothing namespace packageName
  where
    displayVersion :: Release -> FloraHTML
    displayVersion release =
      li_ [class_ "release"] $ do
        let versionClass = "release-version" <> if Just True == release.deprecated then " release-deprecated" else ""
        let dataText = ([dataText_ "This release is deprecated, pick another one" | Just True == release.deprecated])
        a_
          ([class_ versionClass, href_ $ Links.versionResource namespace packageName release.version] <> dataText)
          (toHtml $ display release.version)
        " "
        case release.uploadedAt of
          Nothing -> ""
          Just ts ->
            span_ [] $ do
              toHtml $ Time.formatTime Time.defaultTimeLocale "%a, %_d %b %Y" ts
              case release.revisedAt of
                Nothing -> span_ [] ""
                Just revisionDate ->
                  span_
                    [ dataText_
                        ("Revised on " <> display (Time.formatTime Time.defaultTimeLocale "%a, %_d %b %Y, %R %EZ" revisionDate))
                    , class_ "revised-date"
                    ]
                    Icons.pen

displayDependencies
  :: (Namespace, PackageName, Version)
  -- ^ The package namespace and name
  -> Word
  -- ^ Number of dependenciesc
  -> Vector DependencyVersionRequirement
  -- ^ (Namespace, Name, Version requirement, Synopsis of the dependency)
  -> FloraHTML
displayDependencies (namespace, packageName, version) numberOfDependencies dependencies =
  li_ [class_ ""] $ do
    h3_ [class_ "package-body-section"] (toHtml $ "Dependencies (" <> display numberOfDependencies <> ")")
    let deps = foldMap renderDependency dependencies
    ul_ [class_ "dependencies"] $
      deps
        <> showAll Dependencies (Just version) namespace packageName

showAll :: Target -> Maybe Version -> Namespace -> PackageName -> FloraHTML
showAll target mVersion namespace packageName = do
  let resource = case target of
        Dependents -> Links.dependentsPage namespace packageName (PositiveUnsafe 1)
        Dependencies -> Links.dependenciesPage namespace packageName (fromJust mVersion)
        Versions -> Links.versionsPage namespace packageName
  a_ [class_ "dependency", href_ resource] "Show all…"

displayInstructions :: Namespace -> PackageName -> Release -> FloraHTML
displayInstructions namespace packageName latestRelease = do
  when (latestRelease.buildType == Custom) customBuildType
  div_ [class_ "flow flow--small"] $ do
    div_ [class_ "flow flow--tiny"] $ do
      input_
        [ class_ "block min-w0 w100"
        , id_ "package-install-string"
        , type_ "text"
        , onfocus_ "this.select();"
        , value_ (formatInstallString packageName latestRelease)
        , readonly_ "readonly"
        ]
      label_ [for_ "package-install-string", class_ "block text-small"] "Add this line in your cabal file"
    TemplateEnv{features} <- ask
    when (isJust features.blobStoreImpl) $ do
      p_ $ do
        let v = display latestRelease.version
            tarballName = display packageName <> "-" <> v <> ".tar.gz"
            tarballLink = "/packages/" <> display namespace <> "/" <> display packageName <> "/" <> v <> "/" <> tarballName
        a_ [class_ "btn btn--tiny", href_ tarballLink, download_ "", title_ ("Download archive " <> tarballName)] $ do
          Icons.download
          "Download Archive"

displayPackageDeprecation :: PackageAlternatives -> FloraHTML
displayPackageDeprecation (PackageAlternatives inFavourOf) =
  div_ [class_ "alert alert--danger"] $ do
    if Vector.null inFavourOf
      then p_ [] "This package has been deprecated"
      else do
        p_ [] "This package has been deprecated in favour of"
        ul_ [] $
          Vector.forM_ inFavourOf $
            \PackageAlternative{namespace, package} ->
              li_ [] $
                a_
                  [href_ $ Links.packageResource namespace package]
                  (text $ display namespace <> "/" <> display package)

displayReleaseDeprecation :: Maybe (Namespace, PackageName, Version) -> FloraHTML
displayReleaseDeprecation mLatestViableRelease =
  div_ [class_ "alert alert--danger"] $ case mLatestViableRelease of
    Nothing -> p_ [] "This release has been deprecated"
    Just (namespace, package, version) -> do
      p_ [] (text "This release has been deprecated in favour of: ")
      a_
        [href_ $ Links.versionResource namespace package version]
        (text $ display namespace <> "/" <> display package <> "-" <> display version)

displayDependents
  :: (Namespace, PackageName)
  -> Word
  -> Vector Package
  -> FloraHTML
displayDependents (namespace, packageName) numberOfDependents dependents =
  li_ [class_ " dependents"] $ do
    h3_ [class_ "package-body-section"] (toHtml $ "Dependents (" <> display numberOfDependents <> ")")
    if Vector.null dependents
      then ""
      else
        let deps = fold $ intercalateVec ", " $ fmap renderDependent dependents
         in if fromIntegral (Vector.length dependents) >= numberOfDependents
              then deps
              else deps <> ", " <> showAll Dependents Nothing namespace packageName

renderDependent :: Package -> FloraHTML
renderDependent Package{name, namespace} = do
  let qualifiedName = toHtml $ display namespace <> "/" <> display name

  a_ [class_ "dependent", href_ $ Links.packageResource namespace name] qualifiedName

renderDependency :: DependencyVersionRequirement -> FloraHTML
renderDependency DependencyVersionRequirement{namespace, packageName, version} = do
  li_ [class_ "dependency"] $ do
    a_ [href_ $ Links.packageResource namespace packageName] (toHtml packageName)
    toHtmlRaw @Text "&nbsp;"
    if version == ">=0"
      then ""
      else toHtml version

displayPackageFlags :: ReleaseFlags -> FloraHTML
displayPackageFlags (ReleaseFlags packageFlags) =
  if Vector.null packageFlags
    then mempty
    else do
      p_ [class_ "text-small color-tertiary leading-short"] "Use the -f option with cabal commands to enable flags"
      ul_ [class_ "flow flow--small", role_ "list"] $
        Vector.forM_ packageFlags displayPackageFlag

displayPackageFlag :: PackageFlag -> FloraHTML
displayPackageFlag MkPackageFlag{flagName, flagDescription, flagDefault} = case flagDescription of
  "" ->
    div_ [class_ "text-small"] $ do
      -- Import for the ".package-flags > *" CSS rule to fire
      span_ [class_ "color-raise text-break"] (toHtml $ Text.pack (Flag.unFlagName flagName))
      " "
      defaultMarker flagDefault
  _ -> details_ [class_ "text-small"] $ do
    summary_ [] $ do
      span_ [class_ "color-raise text-break"] (toHtml $ Text.pack (Flag.unFlagName flagName))
      span_ $ toHtmlRaw ("&nbsp;" :: Text)
      defaultMarker flagDefault
    div_ [class_ "prose text-break"] $ renderHaddock $ Text.pack flagDescription

defaultMarker :: Bool -> FloraHTML
defaultMarker True = em_ [class_ "text-small"] "(on by default)"
defaultMarker False = em_ [class_ "text-small"] "(off by default)"

intercalateVec :: a -> Vector a -> Vector a
intercalateVec sep vector =
  if Vector.null vector
    then vector
    else Vector.tail $ Vector.concatMap (\word -> Vector.fromList [sep, word]) vector

formatInstallString :: PackageName -> Release -> Text
formatInstallString packageName Release{version} =
  Text.pack
    . render
    $ hcat [pretty packageName, PP.space, rangedVersion, ","]
  where
    rangedVersion :: Doc
    rangedVersion = "^>=" <> majMin
    majMin :: Doc
    majMin =
      if List.head (versionNumbers version) == 0
        then pretty $ mkVersion $ List.take 3 $ versionNumbers version
        else pretty $ mkVersion $ List.take 2 $ versionNumbers version

showPackageSecurityPage
  :: Release
  -> Word
  -> Word
  -> Package
  -> Word
  -> Vector PackageAdvisoryPreview
  -> Bool
  -> FloraHTML
showPackageSecurityPage latestRelease numberOfDependencies numberOfDependents package numberOfReleases advisoryPreviews isLatestViableRelease = do
  presentationHeader
    numberOfReleases
    latestRelease
    numberOfDependencies
    numberOfDependents
    package
    mempty
    "security"
    False
    isLatestViableRelease
  section_ [class_ "wrapper inset-large flow", id_ "content"] $ do
    h2_ [class_ "title-2"] "Security Advisories"
    packageAdvisoriesListing False advisoryPreviews

packageAdvisoriesListing :: Bool -> Vector PackageAdvisoryPreview -> FloraHTML
packageAdvisoriesListing specifyPackage advisoryPreviews =
  if Vector.null advisoryPreviews
    then p_ [] "No advisories found."
    else div_ [class_ "advisory-list"] $ do
      div_ [class_ "advisory-list__head"] $ do
        div_ [class_ "advisory-list__header"] "ID"
        when specifyPackage $
          div_ [class_ "advisory-list__header"] "Package"
        div_ [class_ "advisory-list__header"] "Summary"
        div_ [class_ "advisory-list__header"] "Published"
        div_ [class_ "advisory-list__header"] "Attributes"
      div_ [class_ "advisory-list__body"] $
        Vector.forM_ advisoryPreviews (\preview -> advisoryListRow specifyPackage preview)

formatUploadTime
  :: UTCTime
  -> UTCTime
  -> Text
formatUploadTime timestamp now =
  let diff = now `Time.diffUTCTime` timestamp
   in Text.pack (toRelativeHumanTime diff)

toRelativeHumanTime :: NominalDiffTime -> String
toRelativeHumanTime diff
  | diff < seconds 30 = "just now"
  | diff < minutes 2 = "1 minute ago"
  | diff < hours 1 = Time.formatTime Time.defaultTimeLocale "%M minutes ago" diff
  | diff < hours 24 = Time.formatTime Time.defaultTimeLocale "%H hours ago" diff
  | diff < days 7 = Time.formatTime Time.defaultTimeLocale "%D days ago" diff
  | diff < days 14 = Time.formatTime Time.defaultTimeLocale "1 week ago" diff
  | diff < months 1 = Time.formatTime Time.defaultTimeLocale "%w weeks ago" diff
  | diff < months 2 = Time.formatTime Time.defaultTimeLocale "1 month ago" diff
  | diff < months 12 = show @Int (diff `div'` months 1) <> " months ago"
  | diff < years 2 = "about 1 year ago"
  | otherwise = show @Int (diff `div'` years 1) <> " years ago"

seconds :: Pico -> NominalDiffTime
seconds = Time.secondsToNominalDiffTime

minutes :: Pico -> NominalDiffTime
minutes n = 60 * seconds n

hours :: Pico -> NominalDiffTime
hours n = 60 * minutes n

days :: Pico -> NominalDiffTime
days n = 24 * hours n

months :: Pico -> NominalDiffTime
months n = 30 * days n

years :: Pico -> NominalDiffTime
years n = 12 * months n

currentSectionLabel :: String -> String
currentSectionLabel sectionId = case sectionId of
  "about" -> "About"
  "versions" -> "Versions"
  "changelog" -> "Changelog"
  "dependencies" -> "Dependencies"
  "dependents" -> "Dependents"
  "security" -> "Security"
  _ -> ""

presentationHeader
  :: Word
  -- ^ Number of releases for the package
  -> Release
  -- ^ Release
  -> Word
  -- ^ Number of dependencies
  -> Word
  -- ^ Number of dependents
  -> Package
  -> Vector PackageGroupName
  -> String
  -> Bool
  -> Bool
  -> FloraHTML
presentationHeader numberOfReleases release numberOfDependencies numberOfDependents package@Package{namespace, name, deprecationInfo} groups sectionId latestViableRelease showVersion =
  header_ [class_ "pageHead"] $ do
    div_ [class_ "wrapper flow flow--large"] $ do
      div_ [class_ "aside gap--large"] $ do
        div_ [class_ "flow"] $ do
          h1_ [class_ "pageHead-title tracking-tight"] $ do
            span_ [class_ "prefix"] $ do
              a_ [href_ (Links.namespacePage package.namespace (PositiveUnsafe 1))] (toHtml $ display namespace)
              toHtmlRaw ("&ThinSpace;/&ThinSpace;" :: Text)
            toHtml package.name
          p_ [class_ "pageHead-subtitle text-break"] (toHtml release.synopsis)
        div_ [class_ "flow flow--small self-center"] $ do
          div_ [class_ "cluster cluster--small items-end"] $ do
            when showVersion $ do
              case release.deprecated of
                Just True -> do
                  span_ [class_ "badge badge--big badge--danger"] $ do
                    Icons.trash
                    "Deprecated release"
                _ ->
                  case deprecationInfo of
                    Just _ ->
                      span_ [class_ "badge badge--big badge--danger"] $ do
                        span_ [class_ "sr-only"] "Version "
                        Icons.trash
                        "Deprecated package"
                    _ -> do
                      when latestViableRelease $
                        span_ [class_ "badge badge--big badge--green"] $ do
                          Icons.check
                          "Latest"
              span_ [class_ "title-2 text-right leading-thin"] $ toHtml release.version
      div_ [class_ "pageHead-tip"] $ do
        -- TODO: [non-urgent] Split tabs in a separate function
        nav_ [class_ "tabs", id_ "subsections", ariaLabel_ "Package sections"] $ do
          a_ ([class_ "tab", href_ (Links.versionResource namespace name release.version)] <> ([ariaCurrent_ "page" | sectionId == "about"])) $ do
            Icons.bookOpenText
            "About"
          a_ ([class_ "tab", href_ (Links.versionsPage namespace name)] <> ([ariaCurrent_ "page" | sectionId == "versions"])) $ do
            Icons.history
            toHtml $ display numberOfReleases <> if numberOfReleases > 1 then " Versions" else " Version"
          a_ ([class_ "tab", href_ ("/" <> toUrlPiece (Links.packageVersionChangelog namespace name release.version))] <> ([ariaCurrent_ "page" | sectionId == "changelog"])) $ do
            Icons.logs
            "Changelog"
          a_ ([class_ "tab", href_ (Links.dependenciesPage namespace name release.version)] <> ([ariaCurrent_ "page" | sectionId == "dependencies"])) $ do
            Icons.folderTree
            toHtml $ display numberOfDependencies <> if numberOfDependencies > 1 then " Dependencies" else " Dependency"
          a_ ([class_ "tab", href_ (Links.dependentsPage namespace name (PositiveUnsafe 1))] <> ([ariaCurrent_ "page" | sectionId == "dependents"])) $ do
            Icons.packageSearch
            toHtml $ display numberOfDependents <> if numberOfDependents > 1 then " Dependents" else " Dependent"
          a_ ([class_ "tab", href_ ("/" <> toUrlPiece (Links.packageSecurity namespace name))] <> ([ariaCurrent_ "page" | sectionId == "security"])) $ do
            Icons.shieldAlert
            "Security"
        div_ [class_ "tabs-mobile", id_ "subsectionsMobile"] $ do
          button_ [class_ "tabs-mobileBtn btn btn--secondary", ariaLabel_ ("Switch section (Current: " <> "About" <> ")"), popovertarget_ "subsectionsMobile-menu"] $ do
            Icons.bookOpenText
            div_ [class_ "flex-grow"] $ do
              div_ [class_ "prefix"] "Current section"
              div_ [] $ toHtml $ currentSectionLabel sectionId
            Icons.chevronUpDown
          nav_ [class_ "dropdown dropdown--full", id_ "subsectionsMobile-menu", ariaLabel_ "Package sections", popover_ ""] $ do
            a_ ([class_ "dropdown-item", href_ (Links.versionResource namespace name release.version)] <> ([ariaCurrent_ "page" | sectionId == "about"])) $ do
              Icons.bookOpenText
              "About"
            a_ ([class_ "dropdown-item", href_ (Links.versionsPage namespace name)] <> ([ariaCurrent_ "page" | sectionId == "versions"])) $ do
              Icons.history
              toHtml $ display numberOfReleases <> if numberOfReleases > 1 then " Versions" else " Version"
            a_ ([class_ "dropdown-item", href_ ("/" <> toUrlPiece (Links.packageVersionChangelog namespace name release.version))] <> ([ariaCurrent_ "page" | sectionId == "changelog"])) $ do
              Icons.logs
              "Changelog"
            a_ ([class_ "dropdown-item", href_ (Links.dependenciesPage namespace name release.version)] <> ([ariaCurrent_ "page" | sectionId == "dependencies"])) $ do
              Icons.folderTree
              toHtml $ display numberOfDependencies <> if numberOfDependencies > 1 then " Dependencies" else " Dependency"
            a_ ([class_ "dropdown-item", href_ (Links.dependentsPage namespace name (PositiveUnsafe 1))] <> ([ariaCurrent_ "page" | sectionId == "dependents"])) $ do
              Icons.packageSearch
              toHtml $ display numberOfDependents <> if numberOfDependents > 1 then " Dependents" else " Dependent"
            a_ ([class_ "dropdown-item", href_ ("/" <> toUrlPiece (Links.packageSecurity namespace name))] <> ([ariaCurrent_ "page" | sectionId == "security"])) $ do
              Icons.packageSearch
              "Security"
