module FloraWeb.Templates.Pages.Search where

import Control.Monad (when)
import Data.Text (Text)
import Data.Vector (Vector)
import Distribution.Types.Version (Version)
import Flora.Model.Package (Namespace, PackageName)
import Flora.Search (SearchAction (..))
import FloraWeb.Components.PackageListHeader (presentationHeader)
import FloraWeb.Components.PaginationNav (paginationNav)
import FloraWeb.Templates
import FloraWeb.Templates.Packages.Listing (packageListing)
import Lucid

showAllPackages :: Word -> Word -> Vector (Namespace, PackageName, Text, Version) -> FloraHTML
showAllPackages count currentPage packagesInfo = do
  div_ [class_ "container"] $ do
    presentationHeader "Packages" "" count
    div_ [class_ "md:col-span-3"] $ packageListing packagesInfo
    paginationNav count currentPage ListAllPackages

showResults :: Text -> Word -> Word -> Vector (Namespace, PackageName, Text, Version) -> FloraHTML
showResults searchString count currentPage packagesInfo = do
  div_ [class_ "container"] $ do
    presentationHeader searchString "" count
    div_ [class_ "md:col-span-3"] $ packageListing packagesInfo
    when (count > 30) $
      paginationNav count currentPage (SearchPackages searchString)
