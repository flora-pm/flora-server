module FloraWeb.Pages.Templates.Admin where

import Data.Text.Display
import Lucid

import Flora.Model.Admin.Report
import FloraWeb.Pages.Templates.Types

index :: AdminReport -> FloraHTML
index adminReport = do
  div_ [class_ "container admin-page"] $ do
    h1_ [class_ "admin-title"] "Overview"
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
          display (adminReport.totalPackages)

    div_ [class_ "admin-card"] $ do
      dt_
        [class_ ""]
        "Total Users"
      dd_ [class_ ""] $
        toHtml $
          display (adminReport.totalUsers)

    div_ [class_ "admin-card"] $ do
      dt_
        [class_ ""]
        "README, CHANGELOG, Upload time…"

      dd_ [class_ ""] $
        form_ [action_ "/admin/metadata", method_ "POST"] $ do
          button_ [class_ ""] "Fetch release metadata"

    div_ [class_ "admin-card"] $ do
      dt_
        [class_ ""]
        "Index import"

      dd_ [class_ ""] $
        form_ [action_ "/admin/index-import", method_ "POST"] $ do
          button_ [class_ ""] "Schedule"

    a_ [href_ "/admin/odd-jobs"] $
      div_ [class_ "admin-card"] $ do
        dt_
          [class_ ""]
          "Odd jobs ui"
        dd_ [class_ ""] $
          button_ [class_ "on-readmes"] "odd jobs ui"
