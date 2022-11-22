module FloraWeb.Templates.Pages.Search where

import Control.Monad (when)
import Data.Text (Text)
import Data.Vector (Vector)
import Lucid

import Data.Maybe (fromJust, isJust)
import Flora.Model.Package (PackageInfo (..))
import Flora.Search (SearchAction (..))
import FloraWeb.Components.PackageListHeader (presentationHeader)
import FloraWeb.Components.PackageListItem
import FloraWeb.Components.PaginationNav (paginationNav)
import FloraWeb.Templates
import FloraWeb.Templates.Packages (packageListing)

showAllPackages :: Word -> Word -> Vector PackageInfo -> FloraHTML
showAllPackages count currentPage packagesInfo = do
  div_ [class_ "container"] $ do
    presentationHeader "Packages" "" count
    div_ [class_ ""] $ packageListing packagesInfo
    paginationNav count currentPage ListAllPackages

showResults :: Text -> Word -> Word -> Maybe PackageInfo -> Vector PackageInfo -> FloraHTML
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
