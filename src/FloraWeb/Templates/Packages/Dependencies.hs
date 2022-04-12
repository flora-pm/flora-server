module FloraWeb.Templates.Packages.Dependencies where

import Data.Text (Text)
import Data.Text.Display (display)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Flora.Model.Package (Namespace, PackageName)
import FloraWeb.Templates (FloraHTML)
import FloraWeb.Templates.Packages.Listing (presentationHeader, prettyPackageName)
import Lucid

showDependencies :: Text -> Vector (Namespace, PackageName, Text, Text) -> FloraHTML
showDependencies searchString packagesInfo = do
  div_ [class_ "container dark:text-gray-100 text-black"] $ do
    presentationHeader searchString (fromIntegral $ Vector.length packagesInfo)
    packageListing packagesInfo

packageListing :: Vector (Namespace, PackageName, Text, Text) -> FloraHTML
packageListing packages = do
  ul_ [class_ "packages-list"] $ do
    Vector.forM_ packages $ \pInfo -> do
      li_ [class_ "packages-list-item xl:text-xl dark:text-gray-200"] $
        showPackage pInfo

showPackage :: (Namespace, PackageName, Text, Text) -> FloraHTML
showPackage (namespace, name, synopsis, versionRange) = do
  a_ [href_ ("/packages/@" <> display namespace <> "/" <> display name)] $ do
    div_ [class_ "text-slate-300 hover:text-slate-200"] $ do
      p_
        [class_ "package-name inline text-link dark:text-link-dark"]
        (toHtml $ prettyPackageName namespace name)
      p_ [class_ "synopsis inline ml-3"] (toHtml synopsis)
    div_ [class_ "text-slate-300 text-sm"] $ toHtml versionRange
