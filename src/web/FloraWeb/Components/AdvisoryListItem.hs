module FloraWeb.Components.AdvisoryListItem
  ( advisoryListRow
  ) where

import Data.Text.Display
import Data.Time qualified as Time
import Lucid
import Security.CVSS (Rating (..), cvssScore)

import Advisories.HsecId.Orphans ()
import Advisories.Model.Affected.Types
import Control.Monad (when)
import Distribution.Orphans.Version ()
import FloraWeb.Components.Pill
import FloraWeb.Pages.Templates.Types

advisoryListRow
  :: PackageAdvisoryPreview
  -> FloraHTML
advisoryListRow preview = do
  let href = "https://haskell.github.io/security-advisories/advisory/" <> display preview.hsecId <> ".html"
  let (rating, score) = cvssScore preview.cvss
  let severity = case rating of
        None -> ratingNone
        Low -> ratingLow score
        Medium -> ratingMedium score
        High -> ratingHigh score
        Critical -> ratingCritical score
  div_ [class_ "package-advisory-list-item"] $ do
    div_ [class_ "package-advisory-list-item__hsec-id md:order-1"] $ do
      a_ [href_ href] (toHtml $ display preview.hsecId)
      span_ [class_ "package-advisory-list-item__inline-published"] $
        toHtml $
          Time.formatTime Time.defaultTimeLocale "%_d %b %Y" preview.published
    div_ [class_ "package-advisory-list-item__summary"] (toHtml preview.summary)
    div_ [class_ "package-advisory-list-item__published"] $
      toHtml $
        Time.formatTime Time.defaultTimeLocale "%_d %b %Y" preview.published
    div_ [class_ "package-advisory-list-item__attributes"] $ do
      severity
      when preview.fixed fixAvailable
