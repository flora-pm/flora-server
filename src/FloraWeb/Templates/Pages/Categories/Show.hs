module FloraWeb.Templates.Pages.Categories.Show where

import Data.Vector (Vector)
import Data.Vector qualified as V
import Lucid

import Flora.Model.Category.Types (Category (..))
import Flora.Model.Package (PackageInfo)
import FloraWeb.Components.PackageListHeader (presentationHeader)
import FloraWeb.Templates (FloraHTML)
import FloraWeb.Templates.Packages (packageListing)

showCategory :: Category -> Vector PackageInfo -> FloraHTML
showCategory Category{name, synopsis} packagesInfo = do
  div_ [class_ "container"] $ do
    presentationHeader name synopsis (fromIntegral $ V.length packagesInfo)
    packageListing packagesInfo
