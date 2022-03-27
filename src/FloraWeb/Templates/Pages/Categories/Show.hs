module FloraWeb.Templates.Pages.Categories.Show where

import Data.Text (Text)
import Data.Text.Display (display)
import Data.Vector (Vector, forM_)
import qualified Data.Vector as V
import Distribution.Types.Version (Version)
import Flora.Model.Category.Types (Category (..))
import Flora.Model.Package (Namespace, PackageName)
import FloraWeb.Templates (FloraHTML)
import FloraWeb.Templates.Packages.Listing as Listing
import Lucid

showCategory :: Category -> Vector (Namespace, PackageName, Text, Version) -> FloraHTML
showCategory category packagesInfo = do
  div_ [class_ "container dark:text-gray-100 text-black"] $ do
    presentationHeader category (fromIntegral $ V.length packagesInfo)
    packageListing packagesInfo

presentationHeader :: Category -> Word -> FloraHTML
presentationHeader Category{name, synopsis} numberOfPackages = do
  div_ [class_ "divider"] $ do
    div_ [class_ "px-4 py-5 sm:px-6 sm:py-24 lg:py-4 lg:px-8"] $
      h2_ [class_ "text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] $ do
        span_ [class_ "headline"] $ toHtml name
        toHtmlRaw @Text "&nbsp;"
        span_ [class_ "dark:text-gray-200 version"] $ toHtml $ display numberOfPackages <> " packages"
    div_ [class_ "synopsis lg:text-xl text-center"] $
      p_ [class_ ""] (toHtml synopsis)

packageListing :: Vector (Namespace, PackageName, Text, Version) -> FloraHTML
packageListing packages = do
  ul_ [class_ "packages-list"] $ do
    forM_ packages $ \pInfo -> do
      li_ [class_ "packages-list-item xl:text-xl dark:text-gray-200"] $
        Listing.showPackage pInfo
