module FloraWeb.Templates.Pages.Search where

import Data.Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import Distribution.Types.Version (Version)
import Flora.Model.Package (Namespace, PackageName)
import FloraWeb.Templates
import FloraWeb.Templates.Packages.Listing (packageListing, searchResultsAside)
import Lucid

showAllPackages :: Vector (Namespace, PackageName, Text, Version) -> FloraHTML
showAllPackages = showResults Nothing

showResults :: Maybe Text -> Vector (Namespace, PackageName, Text, Version) -> FloraHTML
showResults searchString packagesInfo = do
  div_ [class_ "container px-2 grid md:grid-cols-4 md:gap-6"] $ do
    div_ [class_ "md:mt-6"] $ searchResultsAside searchString (fromIntegral $ V.length packagesInfo)
    div_ [class_ "md:col-span-3"] $ packageListing packagesInfo
