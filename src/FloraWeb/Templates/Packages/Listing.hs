module FloraWeb.Templates.Packages.Listing
  ( packageListing
  , packageListingWithRange
  , showPackageWithRange
  , showPackageWithVersion
  )
where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Types.Version (Version)
import Lucid

import Distribution.Orphans ()
import Distribution.SPDX.License qualified as SPDX
import Flora.Model.Package
import FloraWeb.Components.PackageListItem (packageListItem, packageListItemWithVersionRange)
import FloraWeb.Templates (FloraHTML)

-- | Render a list of package informations
packageListing :: Vector (Namespace, PackageName, Text, Version, SPDX.License) -> FloraHTML
packageListing packages = do
  ul_ [class_ "package-list space-y-2"] $ do
    Vector.forM_ packages $ \pInfo -> do
      showPackageWithVersion pInfo

packageListingWithRange :: Vector (Namespace, PackageName, Text, Version, Text, SPDX.License) -> FloraHTML
packageListingWithRange packages = do
  ul_ [class_ "package-list space-y-2"] $ do
    Vector.forM_ packages $ \pInfo -> do
      showPackageWithRange pInfo

showPackageWithVersion :: (Namespace, PackageName, Text, Version, SPDX.License) -> FloraHTML
showPackageWithVersion (namespace, name, synopsis, version, license) =
  packageListItem (namespace, name, synopsis, version, license)

showPackageWithRange :: (Namespace, PackageName, Text, Version, Text, SPDX.License) -> FloraHTML
showPackageWithRange (namespace, name, versionRange, _latestVersionOfDependency, synopsis, license) =
  packageListItemWithVersionRange (namespace, name, synopsis, versionRange, license)
