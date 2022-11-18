{-# LANGUAGE LambdaCase #-}

module FloraWeb.Templates.Pages.Packages where

import Data.Foldable (fold, forM_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Time (defaultTimeLocale)
import Data.Time qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Debug.Trace
import Distribution.Compiler (CompilerFlavor)
import Distribution.Pretty (pretty)
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.Flag (PackageFlag (..))
import Distribution.Types.Flag qualified as Flag
import Distribution.Types.VersionRange.Internal (VersionRange (..))
import Distribution.Version
import Flora.Model.Category.Types (Category (..))
import Flora.Model.Package.Types
  ( Namespace
  , Package (..)
  , PackageName (..)
  )
import Flora.Model.Release.Types (Release (..), ReleaseMetadata (..), TextHtml (..))
import FloraWeb.Links qualified as Links
import FloraWeb.Templates.Haddock (renderHaddock)
import FloraWeb.Templates.Types (FloraHTML)
import Lucid
import Lucid.Base (makeAttribute, relaxHtmlT)
import Lucid.Orphans ()
import Lucid.Svg (clip_rule_, d_, fill_, fill_rule_, path_, viewBox_)
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

showPackage
  :: Release
  -> Vector Release
  -> Word
  -> Package
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
  dependents
  numberOfDependents
  dependencies
  numberOfDependencies
  categories =
    div_ [class_ "larger-container"] $ do
      presentationHeader latestRelease namespace name (latestRelease.metadata.synopsis)
      packageBody
        package
        latestRelease
        packageReleases
        numberOfReleases
        dependencies
        numberOfDependencies
        dependents
        numberOfDependents
        categories

presentationHeader :: Release -> Namespace -> PackageName -> Text -> FloraHTML
presentationHeader release namespace name synopsis = div_ [class_ "divider"] $ do
  div_ [class_ "page-title"] $
    h1_ [class_ "package-title text-center tracking-tight"] $ do
      span_ [class_ "headline"] $ toHtml (display namespace) <> "/" <> toHtml name
      span_ [class_ "dark:text-gray-200 version"] $ displayReleaseVersion release.version
  div_ [class_ "synopsis lg:text-xl text-center"] $
    p_ [class_ ""] (toHtml synopsis)

packageBody
  :: Package
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
  Package{namespace, name = packageName}
  latestRelease@Release{metadata, version}
  packageReleases
  numberOfReleases
  dependencies
  numberOfDependencies
  dependents
  numberOfDependents
  categories =
    div_ $ div_ [class_ "package-body md:flex"] $ do
      div_ [class_ "package-left-column"] $ ul_ [class_ "package-left-rows grid-rows-3 md:sticky md:top-28"] $ do
        displayCategories categories
        displayLicense (metadata.license)
        displayMaintainer (metadata.maintainer)
        displayLinks namespace packageName latestRelease metadata
        displayVersions namespace packageName packageReleases numberOfReleases
      div_ [class_ "release-readme-column grow"] $ div_ [class_ "grid-rows-3 release-readme"] $ displayReadme latestRelease
      div_ [class_ "package-right-column"] $ ul_ [class_ "package-right-rows grid-rows-3 md:sticky md:top-28"] $ do
        displayInstructions packageName latestRelease
        displayTestedWith latestRelease.metadata.testedWith
        displayDependencies (namespace, packageName, version) numberOfDependencies dependencies
        displayDependents (namespace, packageName) numberOfDependents dependents
        displayPackageFlags metadata.flags

displayReadme :: Release -> FloraHTML
displayReadme release =
  case readme release of
    Nothing -> renderDescription release.metadata.description
    Just (MkTextHtml readme) -> relaxHtmlT readme

renderDescription :: Text -> FloraHTML
renderDescription input = renderHaddock input

displayReleaseVersion :: Version -> FloraHTML
displayReleaseVersion = toHtml

displayLicense :: SPDX.License -> FloraHTML
displayLicense license = li_ [class_ ""] $ do
  div_ [class_ "license"] $ h3_ [class_ "lg:text-2xl package-body-section"] "License"
  p_ [class_ "package-body-section__license"] $ toHtml license

displayCategories :: Vector Category -> FloraHTML
displayCategories categories = li_ [class_ ""] $ do
  div_ [class_ "license "] $ h3_ [class_ "lg:text-2xl package-body-section"] "Categories"
  ul_ [class_ "categories"] $ foldMap renderCategory categories

displayLinks :: Namespace -> PackageName -> Release -> ReleaseMetadata -> FloraHTML
displayLinks namespace packageName release meta@ReleaseMetadata{..} = li_ [class_ ""] $ do
  h3_ [class_ "lg:text-2xl package-body-section links"] "Links"
  ul_ [class_ "links"] $ do
    li_ [class_ "package-link"] $ a_ [href_ (getHomepage meta)] "Homepage"
    li_ [class_ "package-link"] $ a_ [href_ ("https://hackage.haskell.org/package/" <> display packageName)] "Documentation"
    li_ [class_ "package-link"] $ displaySourceRepos sourceRepos
    li_ [class_ "package-link"] $ displayChangelog namespace packageName release.version release.changelog

displaySourceRepos :: Vector Text -> FloraHTML
displaySourceRepos x
  | Vector.null x = toHtml @Text "No source repository"
  | otherwise = a_ [href_ (Vector.head x)] "Source repository"

displayChangelog :: Namespace -> PackageName -> Version -> Maybe TextHtml -> FloraHTML
displayChangelog _ _ _ Nothing = toHtml @Text ""
displayChangelog namespace packageName version (Just _) = a_ [href_ ("/" <> toUrlPiece (Links.packageVersionChangelog namespace packageName version))] "Changelog"

displayVersions :: Namespace -> PackageName -> Vector Release -> Word -> FloraHTML
displayVersions namespace packageName versions numberOfReleases =
  li_ [class_ ""] $ do
    h3_ [class_ "lg:text-2xl package-body-section versions"] "Versions"
    ul_ [class_ "package-versions"] $ do
      Vector.forM_ versions displayVersion
      if fromIntegral (Vector.length versions) >= numberOfReleases
        then ""
        else showAll Versions Nothing namespace packageName
  where
    displayVersion :: Release -> FloraHTML
    displayVersion release =
      li_ [class_ "release"] $ do
        a_
          [class_ "release-version", href_ ("/" <> toUrlPiece (Links.packageVersionLink namespace packageName (release.version)))]
          (toHtml $ display (release.version))
        " "
        case release.uploadedAt of
          Nothing -> ""
          Just ts ->
            span_ [] (toHtml $ Time.formatTime defaultTimeLocale "%a, %_d %b %Y" ts)

displayDependencies
  :: (Namespace, PackageName, Version)
  -- ^ The package namespace and name
  -> Word
  -- ^ Number of dependenciesc
  -> Vector (Namespace, PackageName, Text)
  -- ^ (Namespace, Name, Version requirement, Synopsis of the dependency)
  -> FloraHTML
displayDependencies (namespace, packageName, version) numberOfDependencies dependencies = li_ [class_ ""] $ do
  h3_ [class_ "package-body-section"] (toHtml $ "Dependencies (" <> display numberOfDependencies <> ")")
  ul_ [class_ "dependencies grid-cols-3"] $ do
    let deps = foldMap renderDependency dependencies
    let numberOfShownDependencies = fromIntegral @Int @Word (Vector.length dependencies)
    if numberOfShownDependencies >= numberOfDependencies
      then deps
      else deps <> showAll Dependencies (Just version) namespace packageName

showAll :: Target -> Maybe Version -> Namespace -> PackageName -> FloraHTML
showAll target mVersion namespace packageName = do
  let resource = case target of
        Dependents -> Links.packageDependents namespace packageName
        Dependencies -> Links.packageDependencies namespace packageName (fromJust mVersion)
        Versions -> Links.packageVersions namespace packageName
  a_ [class_ "dependency", href_ ("/" <> toUrlPiece resource)] "Show all…"

displayInstructions :: PackageName -> Release -> FloraHTML
displayInstructions packageName latestRelease = li_ [class_ ""] $ do
  h3_ [class_ "package-body-section"] "Installation"
  div_ [class_ "items-top"] $ div_ [class_ "space-y-2"] $ do
    label_ [for_ "install-string", class_ "font-light"] "In your cabal file:"
    input_
      [ class_ "package-install-string"
      , type_ "text"
      , onfocus_ "this.select();"
      , value_ (formatInstallString packageName latestRelease)
      , readonly_ "readonly"
      ]

displayTestedWith :: Vector Version -> FloraHTML
displayTestedWith compilersVersions
  | Vector.null compilersVersions = mempty
  | otherwise = do
      li_ [class_ ""] $ do
        h3_ [class_ "package-body-section"] "Tested Compilers"
        ul_ [class_ "compiler-badges"] $
          forM_ compilersVersions $ \version -> do
            li_ [] $
              a_ [class_ "compiler-badge"] $
                span_
                  []
                  (toHtml @Text (display version))

displayMaintainer :: Text -> FloraHTML
displayMaintainer maintainerInfo = li_ [class_ ""] $ do
  h3_ [class_ "package-body-section"] "Maintainer"
  p_ [class_ "maintainer-info"] (toHtml maintainerInfo)

displayDependents
  :: (Namespace, PackageName)
  -> Word
  -> Vector Package
  -> FloraHTML
displayDependents (namespace, packageName) numberOfDependents dependents = li_ [class_ " dependents"] $ do
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

getHomepage :: ReleaseMetadata -> Text
getHomepage ReleaseMetadata{..} =
  case homepage of
    Just page -> page
    Nothing ->
      if Vector.null sourceRepos
        then "⚠  No homepage provided"
        else Vector.head sourceRepos

displayPackageFlags :: Vector PackageFlag -> FloraHTML
displayPackageFlags packageFlags =
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
displayPackageFlag MkPackageFlag{flagName, flagDescription, flagDefault} = do
  details_ [] $ do
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
  svg_ [xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 20 20", fill_ "currentColor", class_ "w-5 h-5 tooltip"] $
    path_
      [ fill_rule_ "evenodd"
      , d_
          "M18 10a8 8 0 11-16 0 8 8 0 0116 0zM8.94 6.94a.75.75 0 11-1.061-1.061\
          \ 3 3 0 112.871 5.026v.345a.75.75 0 01-1.5 0v-.5c0-.72.57-1.172 1.081-1.287A1.5 1.5 0 108.94 6.94zM10\
          \ 15a1 1 0 100-2 1 1 0 000 2z"
      , clip_rule_ "evenodd"
      ]

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
  pack . render $
    hcat [pretty packageName, PP.space, rangedVersion, ","]
  where
    rangedVersion :: Doc
    rangedVersion = "^>=" <> pretty version

areVersionRangesAllStrictEq :: Vector VersionRange -> Bool
areVersionRangesAllStrictEq = Vector.all $
  \case
    ThisVersion _ -> True
    _ -> False

transformStrictversions :: Vector Version -> (Version, Version)
transformStrictversions vec = (minimum vec, maximum vec)

groupByCompiler :: Vector (CompilerFlavor, VersionRange) -> Vector (CompilerFlavor, Vector VersionRange)
groupByCompiler vec = Vector.fromList $ Map.toList (traceShowId result)
  where
    result = go Map.empty vec
    go :: Map CompilerFlavor (Vector VersionRange) -> Vector (CompilerFlavor, VersionRange) -> Map CompilerFlavor (Vector VersionRange)
    go acc vec' =
      case Vector.uncons vec' of
        Nothing -> acc
        Just ((compiler, versionRange), rest) ->
          let newAcc =
                Map.alter
                  ( \case
                      Nothing -> Just $ Vector.singleton versionRange
                      Just vector -> Just $ Vector.cons versionRange vector
                  )
                  compiler
                  acc
           in go newAcc rest

unsafeExtractVersions :: Vector VersionRange -> Vector Version
unsafeExtractVersions =
  Vector.map
    ( \case
        ThisVersion v -> v
        _ -> error "Not a strict ThisVersion"
    )
