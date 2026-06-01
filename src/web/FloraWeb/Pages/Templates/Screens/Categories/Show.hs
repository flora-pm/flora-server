module FloraWeb.Pages.Templates.Screens.Categories.Show where

import Data.Vector (Vector)
import Data.Vector qualified as V
import Lucid

import Flora.Model.Category.Types (Category (..))
import Flora.Model.Package.Types (PackageInfo)
import FloraWeb.Components.PackageListHeader (presentationHeader)
import FloraWeb.Pages.Templates (FloraHTML)
import FloraWeb.Pages.Templates.Packages (packageListing)

showCategory :: Category -> Vector PackageInfo -> FloraHTML
showCategory Category{name, synopsis} packagesInfo = do
  let title = (toHtml name)
  presentationHeader title synopsis (fromIntegral $ V.length packagesInfo)
  section_ [class_ "wrapper inset-large flow flow--large"] $ do
    packageListing Nothing packagesInfo
