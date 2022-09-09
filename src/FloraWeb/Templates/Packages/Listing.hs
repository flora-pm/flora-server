module FloraWeb.Templates.Packages.Listing
  ( packageListing
  , packageListingWithRange
  , showPackageWithRange
  , showPackageWithVersion
  )
where

import Data.Text (Text)
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Types.Version (Version)
import Lucid

import Distribution.Orphans ()
import Flora.Model.Package
import FloraWeb.Components.PackageListItem (packageListItem)
import FloraWeb.Templates (FloraHTML)

-- | Render a list of package informations
packageListing :: Vector (Namespace, PackageName, Text, Version) -> FloraHTML
packageListing packages = do
  ul_ [class_ "package-list space-y-2"] $ do
    Vector.forM_ packages $ \pInfo -> do
      showPackageWithVersion pInfo

packageListingWithRange :: Vector (Namespace, PackageName, Text, Version, Text) -> FloraHTML
packageListingWithRange packages = do
  ul_ [class_ "package-list space-y-2"] $ do
    Vector.forM_ packages $ \pInfo -> do
      showPackageWithRange pInfo

showPackageWithVersion :: (Namespace, PackageName, Text, Version) -> FloraHTML
showPackageWithVersion (namespace, name, synopsis, version) =
  packageListItem (namespace, name, synopsis, display version)

showPackageWithRange :: (Namespace, PackageName, Text, Version, Text) -> FloraHTML
showPackageWithRange (namespace, name, versionRange, _latestVersionOfDependency, synopsis) =
  packageListItem (namespace, name, synopsis, range)
  where
    range =
      if versionRange == ">=0"
        then ""
        else versionRange
