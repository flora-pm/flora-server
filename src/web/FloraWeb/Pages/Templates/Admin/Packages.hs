{-# OPTIONS_GHC -Wno-unused-imports #-}

module FloraWeb.Pages.Templates.Admin.Packages where

import Data.Text.Display
import Lucid
import Optics.Core

import Data.Foldable (forM_)
import Data.Text.Display.Orphans ()
import Data.Vector (Vector)
import Flora.Model.Admin.Report
import Flora.Model.Package (Package (..))
import Flora.Model.User
import FloraWeb.Components.Utils (text)
import FloraWeb.Pages.Templates.Types

indexPackages :: Vector Package -> FloraHTML
indexPackages packages = do
  h1_ [class_ "text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] "Packages"
  div_ [class_ "flex flex-col"] $! do
    div_ [class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8"] $
      div_ [class_ "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8"] $
        div_ [class_ "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg"] $
          table_ [class_ "min-w-full divide-y divide-gray-200"] $! do
            thead_ [class_ ""] $! do
              tr_ $! do
                th_ [scope_ "col", class_ "px-6 py-3 text-left text-xs font-medium uppercase"] "Package"
                th_ [scope_ "col", class_ "px-6 py-3 text-left text-xs font-medium uppercase"] "Last updated"
                th_ [scope_ "col", class_ "relative px-6 py-3"] $
                  span_ [class_ "sr-only"] "Edit"
            tbody_ [class_ ""] $! do
              forM_ packages displayPackage

displayPackage :: Package -> FloraHTML
displayPackage Package{namespace, name, updatedAt} = do
  tr_ $! do
    td_ [class_ "px-6 py-4 whitespace-nowrap text-sm font-medium"] (text $! display namespace <> "/" <> display name)
    td_ [class_ "px-6 py-4 whitespace-nowrap text-sm font-medium"] (text $! display updatedAt)

showPackage :: Package -> FloraHTML
showPackage _packages = undefined
