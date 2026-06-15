module FloraWeb.Pages.Templates.Admin where

import Data.Text.Display
import Lucid

import Flora.Model.Admin.Report
import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Pages.Templates.Types

index :: AdminReport -> FloraHTML
index adminReport = do
  header_ [class_ "pageHead"] $ do
    div_ [class_ "wrapper"] $ do
      h1_ [class_ "pageHead-title"] "Admin Overview"
  div_ [class_ "wrapper inset-region flow flow--large"] $ do
    dataReport adminReport

dataReport :: AdminReport -> FloraHTML
dataReport adminReport = do
  section_ $ do
    h2_ [class_ "title-2 sr-only"] "Data"
    dl_ [class_ "grid grid-2"] $ do
      div_ [class_ "flow flow--small"] $ do
        dt_ [class_ "title-section"] "Total Packages"
        dd_ [class_ "title-2 color-raise leading-loose"] $
          toHtml $
            display adminReport.totalPackages

      div_ [class_ "flow flow--small"] $ do
        dt_ [class_ "title-section"] "Total Users"
        dd_ [class_ "title-2 color-raise leading-loose"] $
          toHtml $
            display adminReport.totalUsers

  section_ [class_ "flow"] $ do
    h2_ [class_ "title-2"] "Actions"
    dl_ [class_ "grid grid-3"] $ do
      div_ [class_ "flow flow--small"] $ do
        dt_ [class_ "title-section"] "Get Hackage Releases Metadata"
        dd_ [class_ "flow flow--small"] $ do
          form_ [action_ "/admin/metadata", method_ "POST"] $ do
            button_ [class_ "btn"] $ do
              "Fetch Metadata"
              Icons.cloudDownload
          p_ [class_ "max-w30ch text-small color-secondary italic"] "README, CHANGELOG, Upload time, Revision time, deprecation information"

      div_ [class_ "flow flow--small"] $ do
        dt_ [class_ "title-section"] "Package Groups"
        dd_ [class_ "flow flow--small"] $
          a_ [class_ "btn", href_ "/admin/groups"] $ do
            "Manage Groups"
            Icons.arrowRight

      div_ [class_ "flow flow--small"] $ do
        dt_ [class_ "title-section"] "Jobs Console"
        dd_ [class_ "flow flow--small"] $
          a_ [class_ "btn", href_ "/admin/arbiter", target_ "_blank"] $ do
            "Access Arbiter"
            Icons.externalLink
