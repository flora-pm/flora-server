module FloraWeb.Pages.Templates.Screens.Search where

import Control.Monad (when)
import Data.Positive
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Vector (Vector)
import Lucid

import Flora.Model.Package (Namespace, PackageInfo (..), PackageInfoWithExecutables (..))
import Flora.Search (SearchAction (..))
import FloraWeb.Components.PackageListHeader (presentationHeader)
import FloraWeb.Components.PaginationNav (paginationNav)
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Packages (packageListing, packageWithExecutableListing)

showAllPackages :: Word -> Positive Word -> Vector PackageInfo -> FloraHTML
showAllPackages count currentPage packagesInfo = do
  div_ [class_ "container"] $ do
    presentationHeader "Packages" "" count
    div_ [class_ ""] $ packageListing Nothing packagesInfo
    paginationNav count currentPage ListAllPackages

showAllPackagesInNamespace
  :: Namespace
  -> Text
  -> Word
  -> Positive Word
  -> Vector PackageInfo
  -> FloraHTML
showAllPackagesInNamespace namespace description count currentPage packagesInfo = do
  div_ [class_ "container"] $ do
    presentationHeader (display namespace) description count
    div_ [class_ ""] $ packageListing Nothing packagesInfo
    paginationNav count currentPage (ListAllPackagesInNamespace namespace)

showResults
  :: Text
  -> Word
  -> Positive Word
  -> Vector PackageInfo
  -- ^ Exact matches
  -> Vector PackageInfo
  -- ^ Results
  -> FloraHTML
showResults searchString count currentPage exactMatches results = do
  div_ [class_ "container"] $ do
    presentationHeader searchString "" count
    packageListing (Just exactMatches) results
    when (count > 30) $
      paginationNav count currentPage (SearchPackages searchString)

showExecutableResults
  :: Text
  -> Word
  -> Positive Word
  -> Vector PackageInfoWithExecutables
  -- ^ Results
  -> FloraHTML
showExecutableResults executableName count currentPage results = do
  div_ [class_ "container"] $ do
    presentationHeader executableName "" count
    packageWithExecutableListing results
    when (count > 30) $
      paginationNav count currentPage (SearchExecutable executableName)
