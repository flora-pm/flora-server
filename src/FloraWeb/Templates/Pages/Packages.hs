module FloraWeb.Templates.Pages.Packages where

import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Display
import Data.Time (defaultTimeLocale)
import qualified Data.Time as Time
import Data.Vector (Vector, forM_)
import qualified Data.Vector as V
import qualified Data.Vector as Vector
import Distribution.Pretty (pretty)
import qualified Distribution.SPDX.License as SPDX
import Flora.Model.Category (Category (..))
import Flora.Model.Package.Types
  ( Namespace
  , Package (..)
  , PackageMetadata (..)
  , PackageName
  )
import Flora.Model.Release (Release (..))
import qualified FloraWeb.Links as Links
import FloraWeb.Templates.Types (FloraHTML)
import Lucid
import Lucid.Orphans ()
import Optics.Core ((^.))
import Servant (ToHttpApiData (..))
import Text.PrettyPrint (Doc, hcat, render)
import qualified Text.PrettyPrint as PP

data Target = Dependents | Dependencies | Versions
  deriving stock (Eq, Ord)

instance Display Target where
  displayBuilder Dependents = "dependents"
  displayBuilder Dependencies = "dependencies"
  displayBuilder Versions = "versions"

showPackage ::
  Release ->
  Vector Release ->
  Word ->
  Package ->
  Vector Package ->
  Word ->
  Vector (Namespace, PackageName, Text) ->
  Word ->
  Vector Category ->
  FloraHTML
showPackage latestRelease packageReleases numberOfReleases package@Package{namespace, name, synopsis} dependents numberOfDependents dependencies numberOfDependencies categories = do
  div_ [class_ "larger-container"] $ do
    presentationHeader latestRelease namespace name (fromMaybe "" synopsis)
    packageBody package latestRelease packageReleases numberOfReleases dependencies numberOfDependencies dependents numberOfDependents categories

presentationHeader :: Release -> Namespace -> PackageName -> Text -> FloraHTML
presentationHeader release namespace name synopsis = do
  div_ [class_ "divider"] $ do
    div_ [class_ "px-4 py-5 sm:px-6 sm:py-24 lg:py-4 lg:px-8"] $
      h2_ [class_ "text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] $ do
        span_ [class_ "headline"] $ "@" <> toHtml namespace <> "/" <> toHtml name
        span_ [class_ "dark:text-gray-200 version"] $ displayReleaseVersion release
    div_ [class_ "synopsis lg:text-xl text-center"] $
      p_ [class_ ""] (toHtml synopsis)

packageBody :: Package -> Release -> Vector Release -> Word -> Vector (Namespace, PackageName, Text) -> Word -> Vector Package -> Word -> Vector Category -> FloraHTML
packageBody Package{namespace, name = packageName, metadata} latestRelease packageReleases numberOfReleases dependencies numberOfDependencies dependents numberOfDependents categories =
  div_ $ do
    div_ [class_ "package-body md:flex"] $ do
      div_ [class_ "package-left-column grow"] $ do
        ul_ [class_ "package-left-rows grid-rows-3"] $ do
          displayCategories categories
          foldMap (displayLicense . license) metadata
          foldMap (displayLinks packageName latestRelease) metadata
          displayVersions namespace packageName packageReleases numberOfReleases
      div_ [class_ "package-right-column md:max-w-xs"] $ do
        ul_ [class_ "package-right-rows grid-rows-3"] $ do
          displayInstructions packageName latestRelease
          foldMap (displayMaintainer . maintainer) metadata
          displayDependencies (namespace, packageName) numberOfDependencies dependencies
          displayDependents (namespace, packageName) numberOfDependents dependents

displayReleaseVersion :: Release -> FloraHTML
displayReleaseVersion Release{version} = toHtml version

displayLicense :: SPDX.License -> FloraHTML
displayLicense license = do
  li_ [class_ "mb-5"] $ do
    div_ [class_ "license mb-3"] $ do
      h3_ [class_ "lg:text-2xl package-body-section"] "License"
    p_ [class_ "package-body-section__license"] $ toHtml license

displayCategories :: Vector Category -> FloraHTML
displayCategories categories = do
  li_ [class_ "mb-5"] $ do
    div_ [class_ "license mb-3"] $ do
      h3_ [class_ "lg:text-2xl package-body-section"] "Categories"
    ul_ [class_ "categories"] $ do
      foldMap renderCategory categories

displayLinks :: PackageName -> Release -> PackageMetadata -> FloraHTML
displayLinks packageName _release meta@PackageMetadata{..} = do
  li_ [class_ "mb-5"] $ do
    h3_ [class_ "lg:text-2xl package-body-section links mb-3"] "Links"
    ul_ [class_ "links"] $ do
      li_ [class_ "package-link"] $ a_ [href_ (getHomepage meta)] "Homepage"
      li_ [class_ "package-link"] $ a_ [href_ ("https://hackage.haskell.org/package/" <> display packageName)] "Documentation"
      li_ [class_ "package-link"] $ displaySourceRepos sourceRepos

displaySourceRepos :: [Text] -> FloraHTML
displaySourceRepos [] = toHtml @Text "No source repository"
displaySourceRepos x = a_ [href_ (head x)] "Source repository"

displayVersions :: Namespace -> PackageName -> Vector Release -> Word -> FloraHTML
displayVersions namespace packageName versions numberOfReleases =
  li_ [class_ "mb-5"] $ do
    h3_ [class_ "lg:text-2xl package-body-section links mb-3"] "Versions"
    ul_ [class_ "package-versions"] $ do
      forM_ versions displayVersion
      if fromIntegral (Vector.length versions) >= numberOfReleases
        then ""
        else showAll (namespace, packageName, Versions)
  where
    displayVersion :: Release -> FloraHTML
    displayVersion release =
      li_ [class_ "package-version"] $ do
        a_
          [href_ ("/" <> toUrlPiece (Links.packageVersionLink namespace packageName (release ^. #version)))]
          (toHtml $ display (release ^. #version))
        case release ^. #uploadedAt of
          Nothing -> ""
          Just ts ->
            span_ [] (toHtml $ Time.formatTime defaultTimeLocale "%a, %_d %b %Y" ts)

displayDependencies ::
  -- | The package namespace and name
  (Namespace, PackageName) ->
  -- | Number of dependencies
  Word ->
  -- | (Namespace, Name, Version requirement)
  Vector (Namespace, PackageName, Text) ->
  FloraHTML
displayDependencies (namespace, packageName) numberOfDependencies dependencies = do
  li_ [class_ "mb-5"] $ do
    h3_ [class_ "lg:text-2xl package-body-section mb-3"] (toHtml $ "Dependencies (" <> display numberOfDependencies <> ")")
    ul_ [class_ "dependencies grid-cols-3"] $ do
      let deps = foldMap renderDependency dependencies
      let numberOfShownDependencies = fromIntegral @Int @Word (Vector.length dependencies)
      if numberOfShownDependencies >= numberOfDependencies
        then deps
        else deps <> showAll (namespace, packageName, Dependencies)

showAll :: (Namespace, PackageName, Target) -> FloraHTML
showAll (namespace, packageName, target) = do
  let resource = "/packages/@" <> display namespace <> "/" <> display packageName <> "/" <> display target
  a_ [class_ "dependency", href_ resource] "Show all…"

displayInstructions :: PackageName -> Release -> FloraHTML
displayInstructions packageName latestRelease = do
  li_ [class_ "mb-5"] $ do
    h3_ [class_ "lg:text-2xl package-body-section mb-3"] "Installation"
    div_ [class_ "items-top"] $ do
      div_ [class_ "space-y-2"] $ do
        label_ [for_ "install-string", class_ "font-light"] "In your cabal file:"
        input_
          [ class_ "package-install-string"
          , type_ "text"
          , id_ "install-string"
          , onfocus_ "this.select();"
          , value_ (formatInstallString packageName latestRelease)
          , readonly_ "readonly"
          ]

displayMaintainer :: Text -> FloraHTML
displayMaintainer maintainerInfo = do
  li_ [class_ "mb-5"] $ do
    h3_ [class_ "lg:text-2xl package-body-section mb-3"] "Maintainer"
    p_ [class_ "maintainer-info"] (toHtml maintainerInfo)

displayDependents ::
  (Namespace, PackageName) ->
  Word ->
  Vector Package ->
  FloraHTML
displayDependents (namespace, packageName) numberOfDependents dependents = do
  li_ [class_ "mb-5 dependents"] $ do
    h3_ [class_ "lg:text-2xl package-body-section dependents mb-3"] (toHtml $ "Dependents (" <> display numberOfDependents <> ")")
    if Vector.null dependents
      then ""
      else
        let deps = fold $ intercalateVec ", " $ fmap renderDependent dependents
         in if fromIntegral (Vector.length dependents) >= numberOfDependents
              then deps
              else deps <> ", " <> showAll (namespace, packageName, Dependents)

renderDependent :: Package -> FloraHTML
renderDependent Package{name, namespace} = do
  let qualifiedName = toHtml $ "@" <> display namespace <> "/" <> display name
  let resource = "/packages/@" <> display namespace <> "/" <> display name

  a_ [class_ "dependent", href_ resource] qualifiedName

renderDependency :: (Namespace, PackageName, Text) -> FloraHTML
renderDependency (namespace, name, version) = do
  let resource = "/packages/@" <> display namespace <> "/" <> display name
  li_ [class_ "dependency"] $ do
    a_ [href_ resource] (toHtml name)
    toHtmlRaw @Text "&nbsp;"
    if version == ">=0"
      then ""
      else toHtml version

renderCategory :: Category -> FloraHTML
renderCategory Category{name, slug} = do
  let resource = "/categories/" <> slug
  li_ [class_ "category"] $ do
    a_ [href_ resource] (toHtml name)

getHomepage :: PackageMetadata -> Text
getHomepage PackageMetadata{..} =
  case homepage of
    Just page -> page
    Nothing ->
      case sourceRepos of
        [] -> "⚠  No homepage provided"
        x -> head x

intercalateVec :: a -> Vector a -> Vector a
intercalateVec sep vector =
  if V.null vector
    then vector
    else V.tail $ V.concatMap (\word -> V.fromList [sep, word]) vector

formatInstallString :: PackageName -> Release -> Text
formatInstallString packageName Release{version} =
  pack . render $
    hcat [pretty packageName, PP.space, rangedVersion, ","]
  where
    rangedVersion :: Doc
    rangedVersion = "^>=" <> pretty version
