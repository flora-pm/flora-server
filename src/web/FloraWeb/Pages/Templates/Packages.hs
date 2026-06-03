module FloraWeb.Pages.Templates.Packages
  ( displayDependencies
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
  , presentationHeaderForSubpage
  , presentationHeaderForVersions
  , presentationHeaderForAdvisories
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
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Algorithms.Intro qualified as MVector
import Distribution.Pretty (pretty)
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.BuildType (BuildType (..))
import Distribution.Types.Flag (PackageFlag (..))
import Distribution.Types.Flag qualified as Flag
import Distribution.Types.Version (Version, mkVersion, versionNumbers)
import Lucid
import Text.PrettyPrint (Doc, hcat, render)
import Text.PrettyPrint qualified as PP

import Advisories.Model.Affected.Types
import Data.Positive
import Distribution.Orphans ()
import Flora.Environment.Env (FeatureEnv (..))
import Flora.Model.Category.Types
import Flora.Model.Package.Types
import Flora.Model.PackageUploader.Types
import Flora.Model.Release.Types
import Flora.Model.Requirement
import Flora.Search (SearchAction (..))
import FloraWeb.Components.AdvisoryListItem
import FloraWeb.Components.Icons qualified as Icon
import FloraWeb.Components.PackageCard
  ( PackageCardProps (..)
  , packageCard
  )
import FloraWeb.Components.PackageListItem
  ( packageListItem
  , packageWithExecutableListItem
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

presentationHeaderForSubpage
  :: Namespace
  -> PackageName
  -> Release
  -> Target
  -> Word
  -> FloraHTML
presentationHeaderForSubpage namespace packageName release target numberOfPackages = div_ [class_ "divider"] $ do
  div_ [class_ "page-title"] $ h1_ [class_ ""] $ do
    span_ [class_ "headline"] $ do
      displayNamespace namespace
      Icon.chevronRightOutline
      linkToPackageWithVersion namespace packageName release.version
      Icon.chevronRightOutline
      toHtml (display target)
  p_ [class_ "synopsis"] $
    span_ [class_ "version"] $
      toHtml $
        display numberOfPackages
          <> " results"

presentationHeaderForVersions
  :: Namespace
  -> PackageName
  -> Word
  -> FloraHTML
presentationHeaderForVersions namespace packageName numberOfReleases = div_ [class_ "divider"] $ do
  div_ [class_ "page-title"] $ h1_ [class_ ""] $ do
    span_ [class_ "headline"] $ do
      displayNamespace namespace
      Icon.chevronRightOutline
      linkToPackage namespace packageName
      Icon.chevronRightOutline
      toHtml (display Versions)
  p_ [class_ "synopsis"] $
    span_ [class_ "version"] $
      toHtml $
        display numberOfReleases
          <> " results"

presentationHeaderForAdvisories
  :: Namespace
  -> PackageName
  -> FloraHTML
presentationHeaderForAdvisories namespace packageName = div_ [class_ "divider"] $ do
  div_ [class_ "page-title"] $ h1_ [class_ ""] $ do
    span_ [class_ "headline"] $ do
      displayNamespace namespace
      Icon.chevronRightOutline
      linkToPackage namespace packageName
      Icon.chevronRightOutline
      toHtml (display Security)

showDependents
  :: Namespace
  -> PackageName
  -> Release
  -> Word
  -> Vector DependencyInfo
  -> Positive Word
  -> FloraHTML
showDependents namespace packageName release count packagesInfo currentPage = do
  -- TODO: Need to be replaced by standardized presentationHeader
  presentationHeaderForSubpage namespace packageName release Dependents count
  section_ [class_ "wrapper inset-large flow", id_ "content"] $ do
    h2_ [class_ "title-2"] "Dependents"
    ul_ [class_ "flow", role_ "list"] $ do
      Vector.forM_
        packagesInfo
        ( \dep ->
            packageListItem
              ( dep.namespace
              , dep.name
              , dep.latestSynopsis
              , dep.latestVersion
              , dep.latestLicense
              , Nothing
              , Nothing
              )
        )
    when (count > 30) $
      paginationNav count currentPage (DependentsOf namespace packageName Nothing)

showDependencies :: Namespace -> PackageName -> Release -> ComponentDependencies -> FloraHTML
showDependencies namespace packageName release componentsInfo = do
  let dependenciesCount = fromIntegral $ Map.foldr (\v acc -> Vector.length v + acc) 0 componentsInfo
  -- TODO: Add standardized presentationHeader here before the section
  section_ [class_ "wrapper inset-large flow", id_ "content"] $ do
    h2_ [class_ "title-2"] "Dependencies"
    ul_ [class_ "flow", role_ "list"] $ do
      requirementListItem componentsInfo

listVersions :: UTCTime -> Namespace -> PackageName -> Vector Release -> FloraHTML
listVersions now namespace packageName releases = do
  -- TODO: Need to be replaced by standardized presentationHeader
  presentationHeaderForVersions namespace packageName (fromIntegral $ Vector.length releases)
  section_ [class_ "wrapper inset-large flow", id_ "content"] $ do
    h2_ [class_ "title-2"] "Version history"
    ul_ [class_ "flow", role_ "list"] $ do
      Vector.forM_
        releases
        (versionListItem now namespace packageName)

versionListItem :: UTCTime -> Namespace -> PackageName -> Release -> FloraHTML
versionListItem now namespace packageName release = do
  let uploadedAt = case release.uploadedAt of
        Nothing -> ""
        Just ts -> do
          let timeLabelFull = display (Time.formatTime Time.defaultTimeLocale "%a, %_d %b %Y, %R %EZ" ts)
          li_ [title_ ("Uploaded: " <> timeLabelFull)] $ do
            span_ [class_ "color-tertiary"] Icon.cloudUpload
            span_ [class_ "sr-only"] "Uploaded: "
            time_ [datetime_ timeLabelFull, title_ ("Uploaded: " <> timeLabelFull)] (toHtml $ formatUploadTime ts now)
  let link = "/packages/" <> display namespace <> "/" <> display packageName <> "/" <> display release.version
  li_ $ do
    a_ [href_ link, class_ "entityCard"] $ do
      div_ [class_ "cluster cluster--tiny"] $ do
        span_ [class_ "entityCard-title"] (toHtml release.version)
        whenJust release.deprecated $ \d -> do
          span_ [class_ "badge badge--danger"] $ do
            span_ [class_ "sr-only"] "Version "
            Icon.trash
            "Deprecated"
      -- TODO: Display on latest non-deprecated release
      -- span_ [class_ "badge badge--green"] $ do
      --   "Latest Release"
      ul_ [class_ "cluster color-secondary text-small", role_ "list"] $ do
        uploadedAt
        case release.revisedAt of
          Nothing -> span_ [] ""
          Just revisionDate -> do
            let timeLabelFull = display (Time.formatTime Time.defaultTimeLocale "%a, %_d %b %Y, %R %EZ" revisionDate)
            li_ [title_ ("Revised: " <> timeLabelFull)] $ do
              span_ [class_ "color-tertiary"] Icon.pen
              span_ [class_ "sr-only"] "Revised: "
              time_ [datetime_ timeLabelFull, title_ ("Revised: " <> timeLabelFull)] (toHtml $ formatUploadTime revisionDate now)
        li_ $ do
          span_ [class_ "color-tertiary"] Icon.scale
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
              , mVersion = Just em.version
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
            , mVersion = Just p.version
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

showChangelog :: Namespace -> PackageName -> Version -> Maybe TextHtml -> FloraHTML
showChangelog namespace packageName version mChangelog = do
  -- TODO: Add standardized presentationHeader here before the section
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

linkToPackageWithVersion :: Namespace -> PackageName -> Version -> FloraHTML
linkToPackageWithVersion namespace packageName version =
  a_
    [ class_ "breadcrumb-segment"
    , href_
        ("/" <> Links.renderLink (Links.packageVersionLink namespace packageName version))
    ]
    (toHtml $ display packageName)

linkToPackage :: Namespace -> PackageName -> FloraHTML
linkToPackage namespace packageName =
  a_
    [ class_ "breadcrumb-segment"
    , href_
        ("/" <> Links.renderLink (Links.packageLink namespace packageName))
    ]
    (toHtml $ display packageName)

displayLicense :: SPDX.License -> FloraHTML
displayLicense license =
  li_ [class_ ""] $ do
    div_ [class_ "license"] $ h3_ [class_ "package-body-section"] "License"
    p_ [class_ "package-body-section__license"] $ toHtml license

displayChangelog :: Namespace -> PackageName -> Version -> Maybe TextHtml -> FloraHTML
displayChangelog _ _ _ Nothing = toHtml @Text ""
displayChangelog namespace packageName version (Just _) = a_ [href_ ("/" <> Links.renderLink (Links.packageVersionChangelog namespace packageName version))] "Changelog"

displaySecurity :: Namespace -> PackageName -> FloraHTML
displaySecurity namespace packageName = a_ [href_ ("/" <> Links.renderLink (Links.packageSecurity namespace packageName))] "Security"

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
                    Icon.pen

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
          Icon.download
          "Download Archive"

displayPackageDeprecation :: PackageAlternatives -> FloraHTML
displayPackageDeprecation (PackageAlternatives inFavourOf) =
  li_ [class_ ""] $ do
    h3_ [class_ "package-body-section release-deprecated"] "Deprecated"
    div_ [class_ "items-top"] $
      div_ [class_ ""] $
        if Vector.null inFavourOf
          then label_ [for_ "install-string", class_ "font-light"] "This package has been deprecated"
          else do
            label_ [for_ "install-string", class_ "font-light"] "This package has been deprecated in favour of"
            ul_ [class_ "package-alternatives"] $
              Vector.forM_ inFavourOf $
                \PackageAlternative{namespace, package} ->
                  li_ [] $
                    a_
                      [href_ $ Links.packageResource namespace package]
                      (text $ display namespace <> "/" <> display package)

displayReleaseDeprecation :: Maybe (Namespace, PackageName, Version) -> FloraHTML
displayReleaseDeprecation mLatestViableRelease =
  li_ [class_ ""] $ do
    h3_ [class_ "package-body-section release-deprecated"] "Deprecated"
    div_ [class_ "items-top"] $ case mLatestViableRelease of
      Nothing -> label_ [for_ "install-string", class_ "font-light"] "This release has been deprecated"
      Just (namespace, package, version) -> do
        label_ [for_ "install-string", class_ "font-light"] (text "This release has been deprecated in favour of: ")
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
      p_ [class_ "text-small color-tertiary leading-short"] $ "Use the -f option with cabal commands to enable flags"
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
  :: Namespace
  -> PackageName
  -> Vector PackageAdvisoryPreview
  -> FloraHTML
showPackageSecurityPage namespace packageName advisoryPreviews = do
  presentationHeaderForAdvisories namespace packageName
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
