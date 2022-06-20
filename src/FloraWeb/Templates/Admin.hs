module FloraWeb.Templates.Admin where

import Data.Text.Display
import Lucid
import Optics.Core

import Flora.Model.Admin.Report
import FloraWeb.Templates.Types

index :: AdminReport -> FloraHTML
index adminReport = do
  div_ [class_ "container dark:text-white text-black py-5"] $ do
    h1_ [class_ "text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] "Overview"
    dataReport adminReport

dataReport :: AdminReport -> FloraHTML
dataReport adminReport = do
  let cardClass = "dark:bg-admin-card shadow rounded-lg overflow-hidden sm:p-6 hover:border-gray-400 focus-within:ring-2 focus-within:ring-offset-2 focus-within:ring-indigo-500"
  dl_ [class_ "mt-5 grid col-space-1 col-end-2 end gap-5 sm:grid-cols-3 py-10"] $ do
    div_ [class_ cardClass] $ do
      dt_
        [class_ "text-sm font-medium truncate"]
        "Total Packages"
      dd_ [class_ "mt-1 text-3xl font-semibold"] $
        toHtml $
          display (adminReport ^. #totalPackages)

    div_ [class_ cardClass] $ do
      dt_
        [class_ "text-sm font-medium truncate"]
        "Total Users"
      dd_ [class_ "mt-1 text-3xl font-semibold"] $
        toHtml $
          display (adminReport ^. #totalUsers)

    div_ [class_ cardClass] $ do
      dt_
        [class_ "text-sm font-medium truncate"]
        "Do the readmes"

      dd_ [class_ "mt-1 text-3xl font-semibold"] $
        form_ [action_ "/admin/readmes", method_ "POST"] $ do
          button_ [class_ "on-readmes"] "Mk readmes"

    a_ [href_ "/admin/odd-jobs"] $
      div_ [class_ cardClass] $ do
        dt_
          [class_ "text-sm font-medium truncate"]
          "Odd jobs ui"
        dd_ [class_ "mt-1 text-3xl font-semibold"] $
          button_ [class_ "on-readmes"] "odd jobs ui"
