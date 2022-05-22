module FloraWeb.Templates.Packages.Dependencies where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Flora.Model.Package (Namespace, PackageName)
import FloraWeb.Components.PackageListHeader (presentationHeader)
import FloraWeb.Templates (FloraHTML)
import FloraWeb.Templates.Packages.Listing (packageListingWithRange)
import Lucid

showDependencies :: Text -> Vector (Namespace, PackageName, Text, Text) -> FloraHTML
showDependencies searchString packagesInfo = do
  div_ [class_ "container dark:text-gray-100 text-black"] $ do
    presentationHeader searchString "" (fromIntegral $ Vector.length packagesInfo)
    div_ [class_ "md:col-span-3"] $ packageListingWithRange packagesInfo
