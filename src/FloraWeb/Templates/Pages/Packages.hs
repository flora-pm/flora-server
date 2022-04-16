module FloraWeb.Templates.Pages.Packages where

import Data.Foldable (fold)
import Data.Text (Text, pack)
import Data.Text.Display
import Data.Vector (Vector)
import qualified Data.Vector as V
import Distribution.Pretty (pretty)
import qualified Distribution.SPDX.License as SPDX
import Lucid
import Lucid.Orphans ()
import Optics.Core ((^.))
import Text.PrettyPrint (Doc, hcat, render)
import qualified Text.PrettyPrint as PP

import qualified Data.Text as T
import qualified Data.Vector as Vector
import Flora.Model.Category (Category (..))
import Flora.Model.Package.Types
  ( Namespace
  , Package (..)
  , PackageMetadata (..)
  , PackageName
  )
import Flora.Model.Release (Release (..))
import FloraWeb.Templates.Types (FloraHTML)
import Lucid.Base (makeAttribute)

data Target = Dependents | Dependencies
  deriving stock (Eq, Ord)

instance Display Target where
  displayBuilder Dependents = "dependents"
  displayBuilder Dependencies = "dependencies"

showPackage ::
  Release ->
  Package ->
  Vector Package ->
  Word ->
  Vector (Namespace, PackageName, Text) ->
  Word ->
  Vector Category ->
  FloraHTML
showPackage latestRelease package@Package{namespace, name, synopsis} dependents numberOfDependents dependencies numberOfDependencies categories = do
  div_ [class_ "container dark:text-gray-100 text-black"] $ do
    presentationHeader latestRelease namespace name synopsis
    packageBody package latestRelease dependencies numberOfDependencies dependents numberOfDependents categories

presentationHeader :: Release -> Namespace -> PackageName -> Text -> FloraHTML
presentationHeader release namespace name synopsis = do
  div_ [class_ "divider"] $ do
    div_ [class_ "px-4 py-5 sm:px-6 sm:py-24 lg:py-4 lg:px-8"] $
      h2_ [class_ "text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] $ do
        span_ [class_ "headline"] $ "@" <> toHtml namespace <> "/" <> toHtml name
        span_ [class_ "dark:text-gray-200 version"] $ displayReleaseVersion release
    div_ [class_ "synopsis lg:text-xl text-center"] $
      p_ [class_ ""] (toHtml synopsis)

packageBody :: Package -> Release -> Vector (Namespace, PackageName, Text) -> Word -> Vector Package -> Word -> Vector Category -> FloraHTML
packageBody Package{namespace, name = packageName, metadata} latestRelease dependencies numberOfDependencies dependents numberOfDependents categories =
  div_ [class_ "package-body"] $ do
    div_ [class_ "grid grid-cols-4 gap-2 mt-8"] $ do
      div_ [class_ "package-left-column"] $ do
        div_ [class_ "grid-rows-3"] $ do
          displayCategories categories
          displayLicense (metadata ^. #license)
          displayLinks packageName latestRelease metadata
      div_ [class_ "col-span-2"] mempty
      div_ [class_ "package-right-column"] $ do
        div_ [class_ "grid-rows-3"] $ do
          displayDependencies (namespace, packageName) numberOfDependencies dependencies
          displayInstructions packageName latestRelease
          displayDependents (namespace, packageName) numberOfDependents dependents

displayReleaseVersion :: Release -> FloraHTML
displayReleaseVersion Release{version} = toHtml version

displayLicense :: SPDX.License -> FloraHTML
displayLicense license = do
  div_ [class_ "mb-5"] $ do
    div_ [class_ "license mb-3"] $ do
      h3_ [class_ "lg:text-2xl package-body-section"] "License"
    p_ [class_ ""] $ toHtml license

displayCategories :: Vector Category -> FloraHTML
displayCategories categories = do
  div_ [class_ "mb-5"] $ do
    div_ [class_ "license mb-3"] $ do
      h3_ [class_ "lg:text-2xl package-body-section"] "Categories"
    ul_ [class_ ""] $ do
      foldMap renderCategory categories

displayLinks :: PackageName -> Release -> PackageMetadata -> FloraHTML
displayLinks packageName _release meta@PackageMetadata{..} = do
  div_ [class_ "mb-5"] $ do
    h3_ [class_ "lg:text-2xl package-body-section links mb-3"] "Links"
    ul_ [class_ "bullets"] $ do
      li_ $ a_ [href_ (getHomepage meta)] "Homepage"
      li_ $ a_ [href_ ("https://hackage.haskell.org/package/" <> display packageName)] "Documentation"
      li_ $ displaySourceRepos sourceRepos

displaySourceRepos :: [Text] -> FloraHTML
displaySourceRepos [] = toHtml @Text "No source repository"
displaySourceRepos x = a_ [href_ (head x)] "Source repository"

displayDependencies ::
  -- | The package namespace and name
  (Namespace, PackageName) ->
  -- | Number of dependencies
  Word ->
  -- | (Namespace, Name, Version requirement)
  Vector (Namespace, PackageName, Text) ->
  FloraHTML
displayDependencies (namespace, packageName) numberOfDependencies dependencies = do
  div_ [class_ "mb-5"] $ do
    h3_ [class_ "lg:text-2xl package-body-section mb-3"] (toHtml $ "Dependencies (" <> display numberOfDependencies <> ")")
    ul_ [class_ "dependencies grid-cols-3"] $ do
      let deps = foldMap renderDependency dependencies
      deps <> showAll (namespace, packageName, Dependencies)

showAll :: (Namespace, PackageName, Target) -> FloraHTML
showAll (namespace, packageName, target) = do
  let resource = "/packages/@" <> display namespace <> "/" <> display packageName <> "/" <> display target
  a_ [class_ "dependency", href_ resource] "Show all…"

displayInstructions :: PackageName -> Release -> FloraHTML
displayInstructions packageName latestRelease = do
  div_ [class_ "mb-5"] $ do
    h3_ [class_ "lg:text-2xl package-body-section mb-3"] "Installation"
    div_ [class_ "items-top"] $ do
      div_ [class_ "space-y-2"] $ do
        label_ [for_ "install-string", class_ "font-light"] "In your .cabal file:"
        input_
          [ class_ "font-mono shadow-sm text-base w-full bg-transparent focus:ring-indigo-500 focus:border-indigo-500 block border-gray-800 dark:border-gray-800 rounded-md"
          , type_ "text"
          , id_ "install-string"
          , onfocus_ "this.select();"
          , value_ (formatInstallString packageName latestRelease)
          , readonly_ "readonly"
          , xModel_ [] "input"
          ]

displayDependents ::
  (Namespace, PackageName) ->
  Word ->
  Vector Package ->
  FloraHTML
displayDependents (namespace, packageName) numberOfDependents dependents = do
  div_ [class_ "mb-5 dependents"] $ do
    h3_ [class_ "lg:text-2xl package-body-section dependents mb-3"] (toHtml $ "Dependents (" <> display numberOfDependents <> ")")
    if Vector.null dependents
      then ""
      else
        let deps = fold $ intercalateVec ", " $ fmap renderDependent dependents
         in deps <> ", " <> showAll (namespace, packageName, Dependents)

renderDependent :: Package -> FloraHTML
renderDependent Package{name, namespace} = do
  let qualifiedName = toHtml $ "@" <> display namespace <> "/" <> display name
  let resource = "/packages/@" <> display namespace <> "/" <> display name

  a_ [class_ "dependency", href_ resource] qualifiedName

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

{- | x-model
 Synchronize a piece of data with an input element
-}
xModel_ ::
  -- | List of x-model modifiers
  [Text] ->
  Text ->
  Attribute
xModel_ mods = case mods of
  [] -> makeAttribute "x-model"
  _ -> makeAttribute ("x-model." <> T.intercalate "." mods)
