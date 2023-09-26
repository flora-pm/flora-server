{-# LANGUAGE QuasiQuotes #-}

module FloraWeb.Pages.Templates.Packages where

import Control.Monad (when)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
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
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.Flag (PackageFlag (..))
import Distribution.Types.Flag qualified as Flag
import Distribution.Types.Version (Version, mkVersion, versionNumbers)
import Lucid
import Lucid.Base
import PyF
import Servant (ToHttpApiData (..))
import Text.PrettyPrint (Doc, hcat, render)
import Text.PrettyPrint qualified as PP

import Data.Foldable (fold)
import Data.Maybe (fromJust)
import Distribution.Pretty (pretty)
import Flora.Model.Category.Types
import Flora.Model.Package
import Flora.Model.Release.Types
import Flora.Model.Requirement
import Flora.Search (SearchAction (..))
import FloraWeb.Components.PackageListItem (licenseIcon, packageListItem, requirementListItem)
import FloraWeb.Components.PaginationNav (paginationNav)
import FloraWeb.Components.Utils (text)
import FloraWeb.Links qualified as Links
import FloraWeb.Pages.Templates (FloraHTML)
import FloraWeb.Pages.Templates.Haddock (renderHaddock)

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
presentationHeaderForSubpage namespace packageName version target numberOfPackages = div_ [class_ "divider"] $ do
  div_ [class_ "page-title"] $ do
    h1_ [class_ ""] $ do
      span_ [class_ "headline"] $ do
        displayNamespace namespace
        chevronRightOutline
        linkToPackageWithVersion namespace packageName version
        chevronRightOutline
        toHtml (display target)
  p_ [class_ "synopsis"] $
    span_ [class_ "version"] $
      toHtml $
        display numberOfPackages <> " results"

presentationHeaderForVersions
  :: Namespace
  -> PackageName
  -> Word
  -> FloraHTML
presentationHeaderForVersions namespace packageName numberOfReleases = div_ [class_ "divider"] $ do
  div_ [class_ "page-title"] $ do
    h1_ [class_ ""] $ do
      span_ [class_ "headline"] $ do
        displayNamespace namespace
        chevronRightOutline
        linkToPackage namespace packageName
        chevronRightOutline
        toHtml (display Versions)
  p_ [class_ "synopsis"] $
    span_ [class_ "version"] $
      toHtml $
        display numberOfReleases <> " results"

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
    div_ [class_ ""] $ do
      ul_ [class_ "package-list"] $
        Vector.forM_
          packagesInfo
          ( \dep ->
              packageListItem (dep.namespace, dep.name, dep.latestSynopsis, dep.latestVersion, dep.latestLicense)
          )
      when (count > 30) $
        paginationNav count currentPage (DependentsOf namespace packageName)

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
    div_ [class_ ""] $
      ul_ [class_ "package-list"] $
        Vector.forM_
          releases
          ( \release -> versionListItem namespace packageName release
          )

versionListItem :: Namespace -> PackageName -> Release -> FloraHTML
versionListItem namespace packageName release = do
  let href = href_ ("/packages/" <> display namespace <> "/" <> display packageName <> "/" <> display release.version)
  let uploadedAt = case release.uploadedAt of
        Nothing -> ""
        Just ts ->
          span_ [class_ "package-list-item__synopsis"] (toHtml $ Time.formatTime defaultTimeLocale "%a, %_d %b %Y" ts)
  li_ [class_ "package-list-item"] $
    a_ [href, class_ ""] $ do
      h4_ [class_ "package-list-item__name"] $
        strong_ [class_ ""] . toHtml $
          "v" <> toHtml release.version
      uploadedAt
      div_ [class_ "package-list-item__metadata"] $
        span_ [class_ "package-list-item__license"] $ do
          licenseIcon
          toHtml release.license

-- | Render a list of package informations
packageListing :: Vector PackageInfo -> FloraHTML
packageListing packages =
  ul_ [class_ "package-list"] $
    Vector.forM_
      packages
      ( \PackageInfo{..} -> packageListItem (namespace, name, synopsis, version, license)
      )

requirementListing :: ComponentDependencies -> FloraHTML
requirementListing requirements =
  ul_ [class_ "component-list"] $ requirementListItem requirements

showChangelog :: Namespace -> PackageName -> Version -> Maybe TextHtml -> FloraHTML
showChangelog namespace packageName version mChangelog = div_ [class_ "container"] $ do
  div_ [class_ "divider"] $ do
    div_ [class_ "page-title"] $
      h1_ [class_ ""] $ do
        span_ [class_ "headline"] $ toHtml ("Changelog of " <> display namespace <> "/" <> display packageName)
        toHtmlRaw @Text "&nbsp;"
        span_ [class_ "version"] $ toHtml $ display version
    section_ [class_ "release-changelog"] $ do
      case mChangelog of
        Nothing -> toHtml @Text "This release does not have a Changelog"
        Just (MkTextHtml changelogText) -> relaxHtmlT changelogText

displayReleaseVersion :: Version -> FloraHTML
displayReleaseVersion = toHtml

-- | Display a namespace with a link
displayNamespace :: Namespace -> FloraHTML
displayNamespace namespace =
  a_
    [ class_ "breadcrumb-segment"
    , href_
        ("/" <> toUrlPiece (Links.namespaceLink namespace 1))
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

displayLinks :: Namespace -> PackageName -> Release -> FloraHTML
displayLinks namespace packageName release =
  li_ [class_ ""] $ do
    h3_ [class_ "package-body-section links"] "Links"
    ul_ [class_ "links"] $ do
      li_ [class_ "package-link"] $ a_ [href_ (getHomepage release)] "Homepage"
      li_ [class_ "package-link"] $ a_ [href_ ("https://hackage.haskell.org/package/" <> display packageName <> "-" <> display release.version)] "Documentation"
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
    Just (MkTextHtml readme) -> relaxHtmlT readme

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
          ([class_ versionClass, href_ ("/" <> toUrlPiece (Links.packageVersionLink namespace packageName release.version))] <> dataText)
          (toHtml $ display release.version)
        " "
        case release.uploadedAt of
          Nothing -> ""
          Just ts ->
            span_ [] $ do
              toHtml $ Time.formatTime defaultTimeLocale "%a, %_d %b %Y" ts
              case release.revisedAt of
                Nothing -> do
                  span_ [] ""
                Just revisionDate -> do
                  span_
                    [ dataText_
                        ( display $
                            Time.formatTime
                              defaultTimeLocale
                              "%a, %_d %b %Y, %R %EZ"
                              revisionDate
                        )
                    , class_ "revised-date"
                    ]
                    pen

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
        Dependents -> Links.packageDependents namespace packageName (PositiveUnsafe 1)
        Dependencies -> Links.packageDependencies namespace packageName (fromJust mVersion)
        Versions -> Links.packageVersions namespace packageName
  a_ [class_ "dependency", href_ ("/" <> toUrlPiece resource)] "Show all…"

displayInstructions :: PackageName -> Release -> FloraHTML
displayInstructions packageName latestRelease =
  li_ [class_ ""] $ do
    h3_ [class_ "package-body-section"] "Installation"
    div_ [class_ "items-top"] $ div_ [class_ ""] $ do
      label_ [for_ "install-string", class_ "font-light"] "In your cabal file:"
      input_
        [ class_ "package-install-string"
        , type_ "text"
        , onfocus_ "this.select();"
        , value_ (formatInstallString packageName latestRelease)
        , readonly_ "readonly"
        ]

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
              Vector.forM_ inFavourOf $ \PackageAlternative{namespace, package} ->
                li_ [] $
                  a_
                    [href_ ("/packages/" <> display namespace <> "/" <> display package)]
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
          [href_ ("/packages/" <> display namespace <> "/" <> display package <> "/" <> display version)]
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
            ( li_ [] . a_ [class_ "compiler-badge"] . toHtml @Text . display
            )

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
  let resource = "/packages/" <> display namespace <> "/" <> display name

  a_ [class_ "dependent", href_ resource] qualifiedName

renderDependency :: (Namespace, PackageName, Text) -> FloraHTML
renderDependency (namespace, name, version) = do
  let resource = "/packages/" <> display namespace <> "/" <> display name
  li_ [class_ "dependency"] $ do
    a_ [href_ resource] (toHtml name)
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
        usageInstructionTooltip
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
    div_ [class_ "package-flag-description"] $ do
      renderHaddock $ Text.pack flagDescription

defaultMarker :: Bool -> FloraHTML
defaultMarker True = em_ "(on by default)"
defaultMarker False = em_ "(off by default)"

---

usageInstructionTooltip :: FloraHTML
usageInstructionTooltip =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor" class="tooltip"> 
  <path fill-rule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zM8.94 6.94a.75.75 0 11-1.061-1.061 3 3 0 112.871 5.026v.345a.75.75 0 01-1.5 0v-.5c0-.72.57-1.172 1.081-1.287A1.5 1.5 0 108.94 6.94zM10 15a1 1 0 100-2 1 1 0 000 2z" clip-rule="evenodd" /> 
</svg> 
|]

chevronRightOutline :: FloraHTML
chevronRightOutline =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="breadcrumb" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor">
    <path stroke-linecap="round" stroke-linejoin="round" d="M8.25 4.5l7.5 7.5-7.5 7.5" stroke="currentColor" /> 
</svg>
|]

pen :: FloraHTML
pen =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="pen">
  <path stroke-linecap="round" stroke-linejoin="round" d="M16.862 4.487l1.687-1.688a1.875 1.875 0 112.652 2.652L6.832 19.82a4.5 4.5 0 01-1.897 1.13l-2.685.8.8-2.685a4.5 4.5 0 011.13-1.897L16.863 4.487zm0 0L19.5 7.125" />
</svg>
|]

-- | @datalist@ element
dataText_ :: Text -> Attribute
dataText_ = makeAttribute "data-text"

intercalateVec :: a -> Vector a -> Vector a
intercalateVec sep vector =
  if Vector.null vector
    then vector
    else Vector.tail $ Vector.concatMap (\word -> Vector.fromList [sep, word]) vector

formatInstallString :: PackageName -> Release -> Text
formatInstallString packageName Release{version} =
  Text.pack . render $
    hcat [pretty packageName, PP.space, rangedVersion, ","]
  where
    rangedVersion :: Doc
    rangedVersion = "^>=" <> majMin
    majMin :: Doc
    majMin = pretty $ mkVersion $ List.take 2 $ versionNumbers version
