module FloraWeb.Templates.Pages.Packages where

import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
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
import Flora.Model.Package.Types (Namespace, Package (..), PackageMetadata (..),
                                  PackageName)
import Flora.Model.Release (Release (..))
import FloraWeb.Templates.Types (FloraHTML)
import Lucid.Base (makeAttribute)


showPackage :: Release -> Package -> Vector Package -> Vector (Namespace, PackageName, Text) -> FloraHTML
showPackage latestRelease package@Package{namespace, name, synopsis} dependents dependencies = do
  div_ [class_ "container dark:text-gray-100 text-black"] $ do
    presentationHeader latestRelease namespace name synopsis
    packageBody package latestRelease dependencies dependents

presentationHeader :: Release -> Namespace -> PackageName -> Text -> FloraHTML
presentationHeader release namespace name synopsis = do
  div_ [class_ "package-header divider"] $ do
    div_ [class_ "px-4 py-4 sm:px-6 sm:py-24 lg:py-4 lg:px-8"] $
      h2_ [class_ "text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] $ do
        span_ [class_ "headline"] $ "@" <> toHtml namespace <> "/" <> toHtml name
        span_ [class_ "dark:text-gray-200 version"] $ displayReleaseVersion release
    div_ [class_ "synopsis lg:text-xl text-center"] $
      p_ [class_ ""] (toHtml synopsis)

packageBody :: Package -> Release -> Vector (Namespace, PackageName, Text) -> Vector Package -> FloraHTML
packageBody Package{name, metadata} latestRelease dependencies dependents =
  div_ [class_ "package-body"] $ do
    div_ [class_ "grid grid-cols-4 gap-2 mt-8"] $ do
      div_ [class_ "package-left-column"] $ do
        div_ [class_ "grid-rows-3"] $ do
          displayLicense (metadata ^. #license)
          displayLinks latestRelease metadata
      div_ [class_ "col-span-2"] mempty
      div_ [class_ "package-right-column"] $ do
        div_ [class_ "grid-rows-3"] $ do
          displayDependencies dependencies
          displayInstructions name latestRelease
          displayDependents dependents

displayReleaseVersion :: Release -> FloraHTML
displayReleaseVersion Release{version} = toHtml version

displayLicense :: SPDX.License -> FloraHTML
displayLicense license = do
  div_ [class_ "mb-5"] $ do
    div_ [class_ "license mb-3"] $ do
      h3_ [class_ "lg:text-2xl package-body-section"] "License"
    p_ [class_ ""] $ toHtml license

displayLinks :: Release -> PackageMetadata -> FloraHTML
displayLinks _release meta@PackageMetadata{..} = do
  div_ [class_ "mb-5"] $ do
    div_ [class_ "links mb-3"] $ do
      h3_ [class_ "lg:text-2xl package-body-section"] "Links"
    ul_ [class_ "bullets"]$ do
      li_ $ a_ [href_ (getHomepage meta)] "Homepage"
      li_ $ a_ [href_ documentation] "Documentation"
      li_ $ a_ [href_ sourceRepo ] "Source repository"

displayDependencies :: Vector (Namespace, PackageName, Text) -- ^ (Namespace, Name, Version requirement)
                    -> FloraHTML
displayDependencies dependencies = do
  div_ [class_ "mb-5"] $ do
    div_ [class_ "links mb-3"] $ do
      h3_ [class_ "lg:text-2xl package-body-section"] "Dependencies"
    ul_ [class_ "dependencies grid-cols-3"] $ do
      foldMap renderDependency dependencies

displayInstructions :: PackageName -> Release -> FloraHTML
displayInstructions packageName latestRelease = do
  div_ [class_ "mb-5"] $ do
    div_ [class_ "links mb-3"] $ do
      h3_ [class_ "lg:text-2xl package-body-section"] "Installation"
    div_  [class_ "items-top"] $ do
      div_ [class_ "space-y-2"] $ do
        label_ [for_ "install-string", class_ "font-light"] "In your .cabal file:"
        input_ [ class_ "font-mono shadow-sm text-base w-full bg-transparent focus:ring-indigo-500 focus:border-indigo-500 block border-gray-800 dark:border-gray-800 rounded-md"
               , type_ "text", id_ "install-string", onfocus_ "this.select();", value_ (formatInstallString packageName latestRelease), readonly_ "readonly", xModel_ [] "input"
               ]

displayDependents :: Vector Package -> FloraHTML
displayDependents dependents = do
  div_ [class_ "mb-5 dependents w-1/4"] $ do
    div_ [class_ "mb-3"] $ do
      h3_ [class_ "lg:text-2xl package-body-section"] "Dependents"
    fold $ intercalateVec ", " $ fmap renderDependent dependents

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
    toHtml version

getHomepage :: PackageMetadata -> Text
getHomepage PackageMetadata{..} = fromMaybe sourceRepo homepage

intercalateVec :: a -> Vector a -> Vector a
intercalateVec sep vector =
  if V.null vector
  then vector
  else V.tail $ V.concatMap (\word -> V.fromList [sep, word]) vector

formatInstallString :: PackageName -> Release -> Text
formatInstallString packageName Release{version} = pack . render $
  hcat [pretty packageName, PP.space, rangedVersion, "," ]
    where
      rangedVersion :: Doc
      rangedVersion = "^>=" <> pretty version

-- | x-model
-- Synchronize a piece of data with an input element
xModel_
  :: [Text] -- ^ List of x-model modifiers
  -> Text
  -> Attribute
xModel_ mods = case mods of
  [] -> makeAttribute "x-model"
  _  -> makeAttribute ("x-model." <> T.intercalate "." mods)
