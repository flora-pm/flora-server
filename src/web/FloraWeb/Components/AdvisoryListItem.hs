module FloraWeb.Components.AdvisoryListItem
  ( advisoryListRow
  ) where

import Control.Monad (when)
import Data.Text.Display
import Data.Time qualified as Time
import Lucid
import Security.CVSS (Rating (..), cvssScore)

import Advisories.HsecId.Orphans ()
import Advisories.Model.Affected.Types
import Distribution.Orphans.Version ()
import FloraWeb.Components.Pill
import FloraWeb.Links qualified as Links
import FloraWeb.Pages.Templates.Types

advisoryListRow
  :: Bool
  -> PackageAdvisoryPreview
  -> FloraHTML
advisoryListRow specifyPackage preview = do
  let href = "https://haskell.github.io/security-advisories/advisory/" <> display preview.hsecId <> ".html"
  let (rating, score) = cvssScore preview.cvss
  let severity = case rating of
        None -> ratingNone
        Low -> ratingLow score
        Medium -> ratingMedium score
        High -> ratingHigh score
        Critical -> ratingCritical score
  div_ [class_ "package-advisory-list-item"] $ do
    div_ [class_ "package-advisory-list-item__hsec-id"] $ do
      a_ [href_ href] (toHtml $ display preview.hsecId)
      span_ [class_ "package-advisory-list-item__inline-published"] $
        toHtml $
          Time.formatTime Time.defaultTimeLocale "%_d %b %Y" preview.published
    when specifyPackage $
      div_ [class_ "package-advisory-list-item__package"] $ do
        let qualifiedName = toHtml $ display preview.namespace <> "/" <> display preview.packageName
        a_ [class_ "", href_ $ Links.packageResource preview.namespace preview.packageName] qualifiedName
    div_ [class_ "package-advisory-list-item__summary"] (toHtml preview.summary)
    div_ [class_ "package-advisory-list-item__published"] $
      toHtml $
        Time.formatTime Time.defaultTimeLocale "%_d %b %Y" preview.published
    div_ [class_ "package-advisory-list-item__attributes"] $ do
      severity
      when preview.fixed fixAvailable
