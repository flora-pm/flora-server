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
  let href = href_ ("/packages/@" <> display namespace <> "/" <> display name)
  let classes = "card text-inherit my-4 md:my-6" 
  a_ [href, class_ classes] $ do
    h3_ [class_"text-brand-purple dark:text-brand-purple-light"] $ do
      strong_ [] . toHtml $ prettyPackageName namespace name
      small_ [class_ "mx-2"] $ "v" <> (toHtml . display $ version)
    p_ [class_ "text-neutral-900 dark:text-gray-200"] $ toHtml synopsis

prettyPackageName :: Namespace -> PackageName -> Text
prettyPackageName namespace name = "@" <> display namespace <> "/" <> display name

packageListing :: Vector (Namespace, PackageName, Text, Version) -> FloraHTML
packageListing packages = do
  ul_ [class_ "packages-list"] $ do
    Vector.forM_ packages $ \pInfo -> do
      li_ [class_ "px-2 md:px-0"] $
        showPackage pInfo

