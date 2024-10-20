module FloraWeb.Pages.Templates.Packages
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
  , displayReleaseVersion
  , displayTestedWith
  , displayVersions
  , listVersions
  , packageListing
  , packageWithExecutableListing
  , presentationHeaderForSubpage
  , presentationHeaderForVersions
  , showChangelog
  , showDependencies
  , showDependents
  ) where

import Control.Monad (when)
import Control.Monad.Extra (whenJust)
import Control.Monad.Reader (ask)
import Data.Foldable (fold, forM_)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.Positive
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Time (defaultTimeLocale)
import Data.Time qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Algorithms.Intro qualified as MVector
import Distribution.Orphans ()
import Distribution.Pretty (pretty)
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.BuildType (BuildType (..))
import Distribution.Types.Flag (PackageFlag (..))
import Distribution.Types.Flag qualified as Flag
import Distribution.Types.Version (Version, mkVersion, versionNumbers)
import Flora.Environment (FeatureEnv (..))
import Flora.Model.Category.Types
import Flora.Model.Package
import Flora.Model.Release.Types
import Flora.Model.Requirement
import Flora.Search (SearchAction (..))
import FloraWeb.Components.Icons qualified as Icon
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
import Lucid
import Servant (ToHttpApiData (..))
import Text.PrettyPrint (Doc, hcat, render)
import Text.PrettyPrint qualified as PP

data Target
  = Dependents
  | Dependencies
  | Versions
  deriving stock (Eq, Ord)

instance Display Target where
  displayBuilder Dependents = "dependents"
  displayBuilder Dependencies = "dependencies"
  displayBuilder Versions = "versions"

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

showDependents
  :: Namespace
  -> PackageName
  -> Release
  -> Word
  -> Vector DependencyInfo
  -> Positive Word
  -> FloraHTML
showDependents namespace packageName release count packagesInfo currentPage =
  div_ [class_ "container"] $ do
    presentationHeaderForSubpage namespace packageName release Dependents count
    ul_ [class_ "package-list"] $ do
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
  div_ [class_ "container"] $ do
    presentationHeaderForSubpage namespace packageName release Dependencies dependenciesCount
    div_ [class_ ""] $ requirementListing componentsInfo

listVersions :: Namespace -> PackageName -> Vector Release -> FloraHTML
listVersions namespace packageName releases =
  div_ [class_ "container"] $ do
    presentationHeaderForVersions namespace packageName (fromIntegral $ Vector.length releases)
    ul_ [class_ "package-list"] $
      Vector.forM_
        releases
        (versionListItem namespace packageName)

versionListItem :: Namespace -> PackageName -> Release -> FloraHTML
versionListItem namespace packageName release = do
  let href = href_ ("/packages/" <> display namespace <> "/" <> display packageName <> "/" <> display release.version)
  let uploadedAt = case release.uploadedAt of
        Nothing -> ""
        Just ts ->
          span_ [class_ "package-list-item__synopsis"] (toHtml $ Time.formatTime defaultTimeLocale "%a, %_d %b %Y" ts)
  li_ [class_ "package-list-item"] $
    a_ [href, class_ ""] $
      do
        h4_ [class_ "package-list-item__name"]
          $ strong_ [class_ (if Just True == release.deprecated then " release-deprecated" else "")]
            . toHtml
          $ "v"
            <> toHtml release.version
        uploadedAt
        case release.revisedAt of
          Nothing -> span_ [] ""
          Just revisionDate ->
            span_
              [ dataText_
                  ("Revised on " <> display (Time.formatTime defaultTimeLocale "%a, %_d %b %Y, %R %EZ" revisionDate))
              , class_ "revised-date"
              ]
              Icon.pen
        div_ [class_ "package-list-item__metadata"] $
          span_ [class_ "package-list-item__license"] $
            do
              Icon.license
              toHtml release.license

-- | Render a list of package information
packageListing
  :: Maybe (Vector PackageInfo)
  -- ^ Priority items that are highlighted,
  -- like exact matches for a search
  -> Vector PackageInfo
  -> FloraHTML
packageListing mExactMatchItems packages =
  ul_ [class_ "package-list"] $ do
    whenJust mExactMatchItems $ \exactMatchItems ->
      forM_ exactMatchItems $ \em ->
        div_ [class_ "exact-match"] $
          packageListItem (em.namespace, em.name, em.synopsis, em.version, em.license, em.uploadedAt, em.revisedAt)
    Vector.forM_
      packages
      (\PackageInfo{..} -> packageListItem (namespace, name, synopsis, version, license, uploadedAt, revisedAt))

packageWithExecutableListing
  :: Vector PackageInfoWithExecutables
  -> FloraHTML
packageWithExecutableListing packages =
  ul_ [class_ "package-list"] $ do
    Vector.forM_ packages packageWithExecutableListItem

requirementListing :: ComponentDependencies -> FloraHTML
requirementListing requirements =
  ul_ [class_ "component-list"] $ requirementListItem requirements

showChangelog :: Namespace -> PackageName -> Version -> Maybe TextHtml -> FloraHTML
showChangelog namespace packageName version mChangelog = div_ [class_ "container"] $ div_ [class_ "divider"] $ do
  div_ [class_ "page-title"] $
    h1_ [class_ ""] $ do
      span_ [class_ "headline"] $ toHtml ("Changelog of " <> display namespace <> "/" <> display packageName)
      toHtmlRaw @Text "&nbsp;"
      span_ [class_ "version"] $ toHtml $ display version
  section_ [class_ "release-changelog"] $ do
    case mChangelog of
      Nothing -> toHtml @Text "This release does not have a Changelog"
      Just changelogText -> toHtml changelogText

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
        ("/" <> toUrlPiece (Links.packageVersionLink namespace packageName version))
    ]
    (toHtml $ display packageName)

linkToPackage :: Namespace -> PackageName -> FloraHTML
linkToPackage namespace packageName =
  a_
    [ class_ "breadcrumb-segment"
    , href_
        ("/" <> toUrlPiece (Links.packageLink namespace packageName))
    ]
    (toHtml $ display packageName)

displayLicense :: SPDX.License -> FloraHTML
displayLicense license =
  li_ [class_ ""] $ do
    div_ [class_ "license"] $ h3_ [class_ "package-body-section"] "License"
    p_ [class_ "package-body-section__license"] $ toHtml license

displayCategories :: Vector Category -> FloraHTML
displayCategories categories =
  li_ [class_ ""] $ do
    div_ [class_ "license "] $ h3_ [class_ "package-body-section"] "Categories"
    ul_ [class_ "categories"] $ foldMap renderCategory categories

displayLinks :: Namespace -> PackageName -> Text -> Release -> FloraHTML
displayLinks namespace packageName packageIndexURL release = do
  li_ [class_ ""] $ do
    h3_ [class_ "package-body-section links"] "Links"
    ul_ [class_ "links"] $ do
      when (release.homepage /= Just "") $
        li_ [class_ "package-link"] $
          a_ [href_ (getHomepage release)] "Homepage"
      li_ [class_ "package-link"] $ a_ [href_ (packageIndexURL <> "/package/" <> display packageName <> "-" <> display release.version)] "Documentation"

      li_ [class_ "package-link"] $ displaySourceRepos release.sourceRepos
      li_ [class_ "package-link"] $ displayChangelog namespace packageName release.version release.changelog

displaySourceRepos :: Vector Text -> FloraHTML
displaySourceRepos x
  | Vector.null x = toHtml @Text "No source repository"
  | otherwise = a_ [href_ (Vector.head x)] "Source repository"

displayChangelog :: Namespace -> PackageName -> Version -> Maybe TextHtml -> FloraHTML
displayChangelog _ _ _ Nothing = toHtml @Text ""
displayChangelog namespace packageName version (Just _) = a_ [href_ ("/" <> toUrlPiece (Links.packageVersionChangelog namespace packageName version))] "Changelog"

displayReadme :: Release -> FloraHTML
displayReadme release =
  case readme release of
    Nothing -> renderHaddock release.description
    Just readme -> toHtml readme

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
              toHtml $ Time.formatTime defaultTimeLocale "%a, %_d %b %Y" ts
              case release.revisedAt of
                Nothing -> span_ [] ""
                Just revisionDate ->
                  span_
                    [ dataText_
                        ("Revised on " <> display (Time.formatTime defaultTimeLocale "%a, %_d %b %Y, %R %EZ" revisionDate))
                    , class_ "revised-date"
                    ]
                    Icon.pen

displayDependencies
  :: (Namespace, PackageName, Version)
  -- ^ The package namespace and name
  -> Word
  -- ^ Number of dependenciesc
  -> Vector (Namespace, PackageName, Text)
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
displayInstructions namespace packageName latestRelease =
  li_ [class_ ""] $ do
    h3_ [class_ "package-body-section", id_ "package-install-section"] $ do
      p_ [] "Installation"
      when (latestRelease.buildType == Custom) customBuildType
    div_ [class_ "items-top"] $ div_ [class_ ""] $ do
      label_ [for_ "install-string", class_ "font-light"] "In your cabal file:"
      input_
        [ class_ "package-install-string"
        , type_ "text"
        , onfocus_ "this.select();"
        , value_ (formatInstallString packageName latestRelease)
        , readonly_ "readonly"
        ]
      TemplateEnv{features} <- ask
      when (isJust features.blobStoreImpl) $ do
        label_ [for_ "tarball", class_ "font-light"] "Download"
        let v = display latestRelease.version
            tarballName = display packageName <> "-" <> v <> ".tar.gz"
            tarballLink = "/packages/" <> display namespace <> "/" <> display packageName <> "/" <> v <> "/" <> tarballName
        div_ $ a_ [href_ tarballLink, download_ ""] $ toHtml tarballName

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

displayTestedWith :: Vector Version -> FloraHTML
displayTestedWith compilersVersions'
  | Vector.null compilersVersions' = mempty
  | otherwise = do
      let compilersVersions = Vector.reverse $ Vector.modify MVector.sort compilersVersions'
      li_ [class_ ""] $ do
        h3_ [class_ "package-body-section"] "Tested Compilers"
        ul_ [class_ "compiler-badges"] $
          Vector.forM_
            compilersVersions
            (li_ [] . a_ [class_ "compiler-badge"] . toHtml @Text . display)

displayMaintainer :: Text -> FloraHTML
displayMaintainer maintainerInfo =
  li_ [class_ ""] $ do
    h3_ [class_ "package-body-section"] "Maintainer"
    p_ [class_ "maintainer-info"] (toHtml maintainerInfo)

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

renderDependency :: (Namespace, PackageName, Text) -> FloraHTML
renderDependency (namespace, name, version) = do
  li_ [class_ "dependency"] $ do
    a_ [href_ $ Links.packageResource namespace name] (toHtml name)
    toHtmlRaw @Text "&nbsp;"
    if version == ">=0"
      then ""
      else toHtml version

renderCategory :: Category -> FloraHTML
renderCategory Category{name, slug} = do
  let resource = "/categories/" <> slug
  li_ [class_ "category"] $ a_ [href_ resource] (toHtml name)

getHomepage :: Release -> Text
getHomepage release =
  case release.homepage of
    Just page -> page
    Nothing ->
      if Vector.null release.sourceRepos
        then "⚠  No homepage provided"
        else Vector.head release.sourceRepos

displayPackageFlags :: ReleaseFlags -> FloraHTML
displayPackageFlags (ReleaseFlags packageFlags) =
  if Vector.null packageFlags
    then mempty
    else do
      h3_ [class_ "package-body-section package-flags-section"] "Package Flags"
      span_
        [ dataText_ "Use the -f option with cabal commands to enable flags"
        , class_ "instruction-tooltip"
        ]
        Icon.usageInstructionTooltip
      ul_ [class_ "package-flags"] $
        Vector.forM_ packageFlags displayPackageFlag

displayPackageFlag :: PackageFlag -> FloraHTML
displayPackageFlag MkPackageFlag{flagName, flagDescription, flagDefault} = case flagDescription of
  "" ->
    div_ [] $ do
      -- Import for the ".package-flags > *" CSS rule to fire
      pre_ [class_ "package-flag-name"] (toHtml $ Text.pack (Flag.unFlagName flagName))
      toHtmlRaw @Text "&nbsp;"
      defaultMarker flagDefault
  _ -> details_ [] $ do
    summary_ [] $ do
      pre_ [class_ "package-flag-name"] (toHtml $ Text.pack (Flag.unFlagName flagName))
      toHtmlRaw @Text "&nbsp;"
      defaultMarker flagDefault
    div_ [class_ "package-flag-description"] $ renderHaddock $ Text.pack flagDescription

defaultMarker :: Bool -> FloraHTML
defaultMarker True = em_ "(on by default)"
defaultMarker False = em_ "(off by default)"

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
