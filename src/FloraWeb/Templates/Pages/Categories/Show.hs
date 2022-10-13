module FloraWeb.Templates.Pages.Categories.Show where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.Version (Version)
import Lucid

import Flora.Model.Category.Types (Category (..))
import Flora.Model.Package (Namespace, PackageName)
import FloraWeb.Components.PackageListHeader (presentationHeader)
import FloraWeb.Templates (FloraHTML)
import FloraWeb.Templates.Packages.Listing (packageListing)

showCategory :: Category -> Vector (Namespace, PackageName, Text, Version, SPDX.License) -> FloraHTML
showCategory Category{name, synopsis} packagesInfo = do
  div_ [class_ "container"] $ do
    presentationHeader name synopsis (fromIntegral $ V.length packagesInfo)
    packageListing packagesInfo
