module FloraWeb.Templates.Admin where

import Optics.Core

import Data.Text.Display
import Lucid

import Flora.Model.Admin.Report
import Flora.Model.User
import FloraWeb.Templates.Types
import Data.Vector (Vector)
import Data.Foldable (forM_)
import FloraWeb.Templates.Layout.App (text)

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
      dt_ [class_ "text-sm font-medium truncate"]
        "Total Packages"
      dd_ [class_ "mt-1 text-3xl font-semibold"] $
        toHtml $ display (adminReport ^. #totalPackages)

    div_ [class_ cardClass] $ do
      dt_ [class_ "text-sm font-medium truncate"]
        "Total Users"
      dd_ [class_ "mt-1 text-3xl font-semibold"] $
        toHtml $ display (adminReport ^. #totalUsers)

indexUsers :: Vector User -> FloraHTML
indexUsers users =
  div_ [class_ "flex flex-col"] $ do
    div_ [class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8"] $
      div_ [class_ "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8"] $
        div_ [class_ "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg"] $
          table_ [class_ "min-w-full divide-y divide-gray-200"] $ do
            thead_ [class_ ""] $ do
              tr_ $ do
                th_ [scope_ "col", class_ "px-6 py-3 text-left text-xs font-medium uppercase tracking-wider"] "Account"
                th_ [scope_ "col", class_ "px-6 py-3 text-left text-xs font-medium uppercase tracking-wider"] "Email"
                th_ [scope_ "col", class_ "px-6 py-3 text-left text-xs font-medium uppercase tracking-wider"] "Role"
                th_ [scope_ "col", class_ "relative px-6 py-3"] $
                  span_ [class_ "sr-only"] "Edit"
            tbody_ [class_ ""] $ do
              displayUsers users

displayUsers :: Vector User -> FloraHTML
displayUsers users = forM_ users displayUser
  where
    displayUser :: User -> FloraHTML
    displayUser User{username, email, userFlags} = do
      tr_ $ do
        td_ [class_ "px-6 py-4 whitespace-nowrap text-sm font-medium"] (text username)
        td_ [class_ "px-6 py-4 whitespace-nowrap text-sm font-medium"] (text email)
        td_ [class_ "px-6 py-4 whitespace-nowrap text-sm font-medium"] $ if userFlags ^. #isAdmin then "Admin" else "User"


showUser :: User -> FloraHTML
showUser _user = undefined
