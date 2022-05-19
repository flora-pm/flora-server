module FloraWeb.Templates.Packages.Listing
  ( searchResultsAside
  , packageListing
  , packageListingWithRange
  , showPackageWithRange
  , showPackageWithVersion
  )
where

import Data.Text (Text)
import Data.Text.Display (display)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Distribution.Types.Version (Version)
import Lucid

import Flora.Model.Package
import Flora.Model.Release.Orphans ()
import FloraWeb.Components.PackageListItem (packageListItem)
import FloraWeb.Templates (FloraHTML)

searchResultsAside :: Maybe Text -> Word -> FloraHTML
searchResultsAside mSearchString numberOfPackages = do
  div_ [class_ "px-4 py-3 sm:px-6 bg-white shadow-sm dark:bg-slate-600 rounded-md flex items-center mb-5"] $ do
    case mSearchString of
      Just searchString -> do
        img_ [src_ "/static/icons/search.svg", class_ "h-6 w-6 mr-2 dark:invert"]
        div_ [] $ do
          h5_ [] $ toHtml searchString
          strong_ [class_ "dark:text-gray-200"] $ toHtml $ display numberOfPackages <> " packages"
      Nothing -> span_ [class_ "dark:text-gray-200"] $ toHtml $ display numberOfPackages <> " packages"

-- | Render a list of package informations
packageListing :: Vector (Namespace, PackageName, Text, Version) -> FloraHTML
packageListing packages = do
  ul_ [class_ "packages-list space-y-2"] $ do
    Vector.forM_ packages $ \pInfo -> do
      showPackageWithVersion pInfo

packageListingWithRange :: Vector (Namespace, PackageName, Text, Text) -> FloraHTML
packageListingWithRange packages = do
  ul_ [class_ "packages-list space-y-2"] $ do
    Vector.forM_ packages $ \pInfo -> do
      showPackageWithRange pInfo

showPackageWithVersion :: (Namespace, PackageName, Text, Version) -> FloraHTML
showPackageWithVersion (namespace, name, synopsis, version) =
  packageListItem (namespace, name, synopsis, display version)

showPackageWithRange :: (Namespace, PackageName, Text, Text) -> FloraHTML
showPackageWithRange (namespace, name, synopsis, versionRange) =
  packageListItem (namespace, name, synopsis, range)
  where
    range =
      if versionRange == ">=0"
        then ""
        else versionRange
