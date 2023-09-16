module FloraWeb.Pages.Templates.Pages.Search where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Vector (Vector)
import Lucid

import Data.Maybe (fromJust, isJust)
import Data.Positive
import Flora.Model.Package (Namespace, PackageInfo (..))
import Flora.Search (SearchAction (..))
import FloraWeb.Components.PackageListHeader (presentationHeader)
import FloraWeb.Components.PackageListItem
import FloraWeb.Components.PaginationNav (paginationNav)
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Packages (packageListing)

showAllPackages :: Word -> Positive Word -> Vector PackageInfo -> FloraHTML
showAllPackages count currentPage packagesInfo = do
  div_ [class_ "container"] $ do
    presentationHeader "Packages" "" count
    div_ [class_ ""] $ packageListing packagesInfo
    paginationNav count currentPage ListAllPackages

showAllPackagesInNamespace :: Namespace -> Word -> Positive Word -> Vector PackageInfo -> FloraHTML
showAllPackagesInNamespace namespace count currentPage packagesInfo = do
  div_ [class_ "container"] $ do
    let title = "Packages in " <> display namespace
    presentationHeader title "" count
    div_ [class_ ""] $ packageListing packagesInfo
    paginationNav count currentPage (ListAllPackagesInNamespace namespace)

showResults :: Text -> Word -> Positive Word -> Maybe PackageInfo -> Vector PackageInfo -> FloraHTML
showResults searchString count currentPage mExactMatch results = do
  div_ [class_ "container"] $ do
    presentationHeader searchString "" count
    when (isJust mExactMatch) $ do
      let em = fromJust mExactMatch
      div_ [class_ "exact-match"] $
        packageListItem (em.namespace, em.name, em.synopsis, em.version, em.license)
    div_ [class_ ""] $ packageListing results
    when (count > 30) $
      paginationNav count currentPage (SearchPackages searchString)
