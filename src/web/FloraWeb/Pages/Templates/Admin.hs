module FloraWeb.Pages.Templates.Admin where

import Data.Text.Display
import Lucid

import Flora.Model.Admin.Report
import FloraWeb.Components.Sidebar
import FloraWeb.Pages.Templates.Types

index :: AdminReport -> FloraHTML
index adminReport = do
  div_ [class_ "admin-page"] $ do
    sidebar
    h1_ [class_ "admin-title"] "Overview"
    div_ [class_ "admin-body"] $
      dataReport adminReport

dataReport :: AdminReport -> FloraHTML
dataReport adminReport = do
  dl_ [class_ "admin-cards"] $ do
    div_ [class_ "admin-card"] $ do
      dt_
        [class_ ""]
        "Total Packages"
      dd_ [class_ ""] $
        toHtml $
          display adminReport.totalPackages

    div_ [class_ "admin-card"] $ do
      dt_
        [class_ ""]
        "Total Users"
      dd_ [class_ ""] $
        toHtml $
          display adminReport.totalUsers

    div_ [class_ "admin-card"] $ do
      dt_
        [class_ ""]
        "README, CHANGELOG, Upload time, Revision time, deprecation information"

      dd_ [class_ ""] $
        form_ [action_ "/admin/metadata", method_ "POST"] $ do
          button_ [class_ ""] "Fetch Hackage releases metadata"

    a_ [href_ "/admin/odd-jobs"] $
      div_ [class_ "admin-card"] $ do
        dt_
          [class_ ""]
          "Odd jobs ui"
        dd_ [class_ ""] $
          button_ [class_ "on-readmes"] "odd jobs ui"
