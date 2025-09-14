module FloraWeb.Pages.Templates.Screens.Categories.Show where

import Data.Vector (Vector)
import Data.Vector qualified as V
import Lucid

import Flora.Model.Category.Types (Category (..))
import Flora.Model.Package (PackageInfo)
import FloraWeb.Components.Icons qualified as Icon
import FloraWeb.Components.PackageListHeader (presentationHeader)
import FloraWeb.Pages.Templates (FloraHTML)
import FloraWeb.Pages.Templates.Packages (packageListing)

showCategory :: Category -> Vector PackageInfo -> FloraHTML
showCategory Category{name, synopsis} packagesInfo = do
  div_ [class_ "container"] $ do
    let title =
          span_ [class_ "headline"] $ do
            "Categories"
            Icon.chevronRightOutline
            toHtml name
    presentationHeader title synopsis (fromIntegral $ V.length packagesInfo)
    packageListing Nothing packagesInfo
