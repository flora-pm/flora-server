module FloraWeb.Templates.Pages.Search where

import Data.Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import Distribution.Types.Version (Version)
import Flora.Model.Package (Namespace, PackageName)
import FloraWeb.Templates
import FloraWeb.Templates.Packages.Listing (packageListing, presentationHeader)
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
