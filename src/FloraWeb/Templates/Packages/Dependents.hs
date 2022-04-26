module FloraWeb.Templates.Packages.Dependents where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Distribution.Types.Version (Version)
import Flora.Model.Package (Namespace, PackageName)
import FloraWeb.Templates (FloraHTML)
import FloraWeb.Templates.Packages.Listing
import Lucid

showDependents :: Text -> Vector (Namespace, PackageName, Text, Version) -> FloraHTML
showDependents searchString packagesInfo = do
  div_ [class_ "container dark:text-gray-100 text-black"] $ do
    searchResultsAside (Just searchString) (fromIntegral $ Vector.length packagesInfo)
    packageListing packagesInfo
