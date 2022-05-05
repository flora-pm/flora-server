module FloraWeb.Templates.Packages.Dependencies where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Flora.Model.Package (Namespace, PackageName)
import FloraWeb.Templates (FloraHTML)
import FloraWeb.Templates.Packages.Listing (searchResultsAside, showPackageWithRange)
import Lucid

showDependencies :: Text -> Vector (Namespace, PackageName, Text, Text) -> FloraHTML
showDependencies searchString packagesInfo = do
  div_ [class_ "container dark:text-gray-100 text-black"] $ do
    searchResultsAside (Just searchString) (fromIntegral $ Vector.length packagesInfo)
    packageListing packagesInfo

packageListing :: Vector (Namespace, PackageName, Text, Text) -> FloraHTML
packageListing packages = do
  ul_ [class_ "packages-list space-y-4"] $ do
    Vector.forM_ packages $ \pInfo -> do
      li_ [class_ "packages-list-item xl:text-xl dark:text-gray-200"] $
        showPackageWithRange pInfo
