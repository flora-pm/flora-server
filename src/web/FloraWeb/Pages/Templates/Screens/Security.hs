module FloraWeb.Pages.Templates.Screens.Security
  ( showNamespaceAdvisories
  , showPackageAdvisories
  , showAdvisory
  )
where

import Control.Monad (unless, when)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Lucid
import Security.CVSS (Rating (..), cvssScore)

import Advisories.Model.Advisory.Types (AdvisoryDAO (..))
import Advisories.Model.Affected.Types
  ( AffectedPackageInfo (..)
  , PackageAdvisoryPreview (..)
  )
import Data.Positive
import Data.Text.HTML qualified (fromText)
import Flora.Domain.Search (SearchAction (..))
import Flora.Model.Package.Types (Namespace, PackageName)
import FloraWeb.Components.PackageListHeader (presentationHeader)
import FloraWeb.Components.PaginationNav (paginationNav)
import FloraWeb.Components.Pill
  ( ratingCritical
  , ratingHigh
  , ratingLow
  , ratingMedium
  , ratingNone
  )
import FloraWeb.Links qualified as Links
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Packages (packageAdvisoriesListing)

showNamespaceAdvisories
  :: Namespace
  -> Word
  -> Positive Word
  -> Vector PackageAdvisoryPreview
  -> FloraHTML
showNamespaceAdvisories namespace count currentPage previews = do
  presentationHeader (toHtml $ "Security advisories for " <> display namespace) "" count
  section_ [class_ "wrapper inset-large flow flow--large", id_ "content"] $ do
    packageAdvisoriesListing True previews
    when (count > 30) $
      paginationNav count currentPage (ListAdvisoriesInNamespace namespace)

showPackageAdvisories
  :: Namespace
  -> PackageName
  -> Word
  -> Positive Word
  -> Vector PackageAdvisoryPreview
  -> FloraHTML
showPackageAdvisories namespace packageName count currentPage previews = do
  presentationHeader
    (toHtml $ "Security advisories for " <> display namespace <> "/" <> display packageName)
    ""
    count
  section_ [class_ "wrapper inset-large flow flow--large", id_ "content"] $ do
    packageAdvisoriesListing False previews
    when (count > 30) $
      paginationNav count currentPage (ListPackageAdvisories namespace packageName)

showAdvisory :: AdvisoryDAO -> Vector AffectedPackageInfo -> FloraHTML
showAdvisory advisory affectedPackages = do
  presentationHeader (toHtml $ display advisory.hsecId) advisory.summary 1
  section_ [class_ "wrapper inset-large flow flow--large", id_ "content"] $ do
    div_ [class_ "advisory-metadata"] $ do
      p_ [class_ "text-small"] $
        toHtml $
          "Published on "
            <> Text.pack (Time.formatTime Time.defaultTimeLocale "%_d %b %Y" advisory.published)
      unless (Vector.null advisory.aliases) $
        p_ [class_ "text-small"] $
          toHtml $
            "Aliases: " <> Text.intercalate ", " (Vector.toList advisory.aliases)
    div_ [class_ "advisory-content"] $
      toHtmlRaw (Data.Text.HTML.fromText advisory.html)
    unless (Vector.null affectedPackages) $ do
      h2_ [class_ "title-2"] "Affected packages"
      ul_ [class_ "advisory-affected-packages"] $
        Vector.forM_ affectedPackages $ \affected -> do
          let (rating, score) = cvssScore affected.cvss
              severity = case rating of
                None -> ratingNone
                Low -> ratingLow score
                Medium -> ratingMedium score
                High -> ratingHigh score
                Critical -> ratingCritical score
          li_ $ do
            a_
              [class_ "entityCard-title", href_ (Links.packageResource affected.namespace affected.packageName)]
              (toHtml $ display affected.namespace <> "/" <> display affected.packageName)
            toHtml (" " :: Text)
            severity
            toHtml
              ((if affected.fixed then " (fixed)" else " (unfixed)") :: Text)
