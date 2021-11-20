module FloraWeb.Templates.Pages.Packages where

import Data.Foldable
import Data.Function (on)
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

import Flora.Model.Package.Types (Namespace, Package (..), PackageMetadata (..),
                                  PackageName)
import Flora.Model.Release (Release (..))
import FloraWeb.Templates.Types (FloraHTML)
import Lucid.Alpine

showPackage :: Package -> Vector Package -> Vector Release -> Vector (Namespace, PackageName, Text) -> FloraHTML
showPackage package@Package{namespace, name, synopsis} dependents releases dependencies = do
  div_ [class_ "container"] $ do
    presentationHeader (latestRelease releases) namespace name synopsis
    packageBody package releases dependencies dependents

presentationHeader :: Release -> Namespace -> PackageName -> Text -> FloraHTML
presentationHeader release namespace name synopsis = do
  div_ [class_ "package-header divider"] $ do
    div_ [class_ "px-4 py-4 sm:px-6 sm:py-24 lg:py-4 lg:px-8"] $
      h2_ [class_ "text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] $ do
        span_ [class_ "dark:text-white headline"] $ "@" <> toHtml namespace <> "/" <> toHtml name
        span_ [class_ "dark:text-gray-200 version"] $ displayReleaseVersion release
    div_ [class_ "synopsis lg:text-xl"] $
      p_ [class_ ""] (toHtml synopsis)

packageBody :: Package -> Vector Release -> Vector (Namespace, PackageName, Text) -> Vector Package -> FloraHTML
packageBody Package{namespace, name, metadata} releases dependencies dependents  = do
  div_ [class_ "grid grid-cols-4 gap-2 mt-8"] $ do
    div_ [class_ "package-left-column"] $ do
      div_ [class_ "grid-rows-3"] $ do
        displayLicense (metadata ^. #license)
        displayLinks (latestRelease releases) metadata
    div_ [class_ "col-span-2"] mempty
    div_ [class_ "package-right-column"] $ do
      div_ [class_ "grid-rows-3"] $ do
        displayDependencies dependencies
        displayInstructions (formatInstallString namespace name (latestRelease releases))
        displayDependents dependents

latestRelease :: Vector Release -> Release
latestRelease rels = V.maximumBy (compare `on` version) rels

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
    ul_ $ do
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

displayInstructions :: Text -> FloraHTML
displayInstructions formattedVersion = do
  div_ [class_ "mb-5"] $ do
    div_ [class_ "links mb-3"] $ do
      h3_ [class_ "lg:text-2xl package-body-section"] "Installation"
    div_  [class_ "items-top dark:text-white"] $ do
      p_ [] $ do
        input_ [ class_ "shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full border-gray-300 rounded-md"
               , type_ "text", onfocus_ "this.select();", value_ formattedVersion, readonly_ "readonly", xModel_ [] "input"
               ]
      -- button_ [type_ "button", xOn_ "click" $ "$clipboard(() => '" <> formattedVersion <> "')"] "C"

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

formatInstallString :: Namespace -> PackageName -> Release -> Text
formatInstallString namespace packageName Release{version} = pack . render $
  hcat [pretty namespace, "/", pretty packageName, PP.space, rangedVersion ]
    where
      rangedVersion :: Doc
      rangedVersion = "^>=" <> pretty version
