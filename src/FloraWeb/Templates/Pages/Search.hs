module FloraWeb.Templates.Pages.Search where

import Data.Text
import Data.Text.Display (display)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Distribution.Types.Version
import Flora.Model.Package (Namespace, PackageName)
import FloraWeb.Templates
import FloraWeb.Templates.Pages.Categories.Show (packageListing)
import Lucid

showResults :: Text -> Vector (Namespace, PackageName, Text, Version) -> FloraHTML
showResults searchString packagesInfo = do
  div_ [class_ "container dark:text-gray-100 text-black"] $ do
    presentationHeader searchString (fromIntegral $ V.length packagesInfo)
    packageListing packagesInfo

showAllPackages :: Vector (Namespace, PackageName, Text, Version) -> FloraHTML
showAllPackages packagesInfo = do
  div_ [class_ "container dark:text-gray-100 text-black"] $ do
    presentationHeader "" (fromIntegral $ V.length packagesInfo)
    packageListing packagesInfo

presentationHeader :: Text -> Word -> FloraHTML
presentationHeader searchString numberOfPackages = do
  div_ [class_ "divider"] $ do
    div_ [class_ "px-4 py-5 sm:px-6 sm:py-24 lg:py-4 lg:px-8"] $
      h2_ [class_ "text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] $ do
        span_ [class_ "headline"] $ toHtml searchString
        toHtmlRaw @Text "&nbsp;"
        span_ [class_ "dark:text-gray-200 version"] $ toHtml $ display numberOfPackages <> " packages"
