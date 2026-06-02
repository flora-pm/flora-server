module FloraWeb.Pages.Templates.Screens.Search where

import Control.Monad (when)
import Data.Text (Text, pack)
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Lucid

import Advisories.Model.Affected.Types
import Data.Positive
import Flora.Model.Package.Types (Namespace, PackageInfo (..), PackageInfoWithExecutables (..))
import Flora.Search (SearchAction (..))
import FloraWeb.Components.PackageListHeader (presentationHeader)
import FloraWeb.Components.PaginationNav (paginationNav)
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Packages (packageAdvisoriesListing, packageListing, packageWithExecutableListing)

showAllPackages :: UTCTime -> Word -> Positive Word -> Vector PackageInfo -> FloraHTML
showAllPackages now count currentPage packagesInfo = do
  let pageCountLabel = if currentPage > 1 then "Page " <> pack (show (currentPage.unPositive)) else ""
  presentationHeader "Packages" pageCountLabel count
  section_ [class_ "wrapper inset-large flow flow--large", id_ "content"] $ do
    p_ [class_ "text-small"] "Displaying 1-50 of 143 total results" -- TODO: Display real numbers
    packageListing now Nothing packagesInfo
    paginationNav count currentPage ListAllPackages

showAllPackagesInNamespace
  :: UTCTime
  -> Namespace
  -> Text
  -> Word
  -> Positive Word
  -> Vector PackageInfo
  -> FloraHTML
showAllPackagesInNamespace now namespace description count currentPage packagesInfo = do
  presentationHeader (toHtml $ display namespace) description count
  section_ [class_ "wrapper inset-large flow flow--large", id_ "content"] $ do
    packageListing now Nothing packagesInfo
    paginationNav count currentPage (ListAllPackagesInNamespace namespace)

showResults
  :: UTCTime
  -> Text
  -> Word
  -> Positive Word
  -> Vector PackageInfo
  -- ^ Exact matches
  -> Vector PackageInfo
  -- ^ Results
  -> FloraHTML
showResults now searchString count currentPage exactMatches results = do
  presentationHeader ("Search for: " <> toHtml searchString) "" count
  section_ [class_ "wrapper inset-large flow flow--large", id_ "content"] $ do
    packageListing now (Just exactMatches) results
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
  presentationHeader (toHtml executableName) "" count
  section_ [class_ "wrapper inset-large flow flow--large", id_ "content"] $ do
    packageWithExecutableListing results
    when (count > 30) $
      paginationNav count currentPage (SearchExecutable executableName)

showAdvisorySearchResults
  :: Text
  -> Word
  -> Positive Word
  -> Vector PackageAdvisoryPreview
  -- ^ Results
  -> FloraHTML
showAdvisorySearchResults searchTerm count currentPage results = do
  presentationHeader (toHtml searchTerm) "" count
  section_ [class_ "wrapper inset-large flow flow--large", id_ "content"] $ do
    packageAdvisoriesListing True results
    when (count > 30) $
      paginationNav count currentPage (SearchInAdvisories searchTerm)
