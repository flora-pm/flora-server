module FloraWeb.Templates.Packages.Listing where

import Data.Text (Text)
import Data.Text.Display (display)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Distribution.Types.Version (Version)
import Lucid

import Flora.Model.Package
import Flora.Model.Release.Orphans ()
import FloraWeb.Templates (FloraHTML)

presentationHeader :: Text -> Word -> FloraHTML
presentationHeader searchString numberOfPackages = do
  div_ [class_ "divider"] $ do
    div_ [class_ "px-4 py-5 sm:px-6 sm:py-24 lg:py-4 lg:px-8"] $
      h2_ [class_ "text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] $ do
        span_ [class_ "headline"] $ toHtml searchString
        toHtmlRaw @Text "&nbsp;"
        span_ [class_ "dark:text-gray-200 version"] $ toHtml $ display numberOfPackages <> " packages"

showPackage :: (Namespace, PackageName, Text, Version) -> FloraHTML
showPackage (namespace, name, synopsis, version) = do
  a_ [href_ ("/packages/@" <> display namespace <> "/" <> display name)] $ do
    div_ [class_ "text-slate-300 hover:text-slate-200"] $ do
      p_
        [class_ "package-name inline text-link dark:text-link-dark"]
        (toHtml $ prettyPackageName namespace name)
      p_ [class_ "synopsis inline ml-3"] (toHtml synopsis)
    div_ [class_ "text-slate-300 text-sm"] $ "v" <> toHtml (display version)

prettyPackageName :: Namespace -> PackageName -> Text
prettyPackageName namespace name = "@" <> display namespace <> "/" <> display name

packageListing :: Vector (Namespace, PackageName, Text, Version) -> FloraHTML
packageListing packages = do
  ul_ [class_ "packages-list"] $ do
    Vector.forM_ packages $ \pInfo -> do
      li_ [class_ "packages-list-item xl:text-xl dark:text-gray-200"] $
        showPackage pInfo

getPackageInfo :: Package -> Version -> (Namespace, PackageName, Text, Version)
getPackageInfo Package{namespace, name, synopsis} version = (namespace, name, synopsis, version)
