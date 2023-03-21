module FloraWeb.Templates.Admin.Users where

import Lucid

import Data.Foldable (forM_)
import Data.Vector (Vector)
import Flora.Model.User
import FloraWeb.Components.Utils (text)
import FloraWeb.Templates.Types

indexUsers :: Vector User -> FloraHTML
indexUsers users = do
  h1_ [class_ "text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] "Users"
  div_ [class_ "flex flex-col"] $! do
    div_ [class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8"] $
      div_ [class_ "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8"] $
        div_ [class_ "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg"] $
          table_ [class_ "min-w-full divide-y divide-gray-200"] $! do
            thead_ [class_ ""] $! do
              tr_ $! do
                th_ [scope_ "col", class_ "px-6 py-3 text-left text-xs font-medium uppercase tracking-wider"] "Account"
                th_ [scope_ "col", class_ "px-6 py-3 text-left text-xs font-medium uppercase tracking-wider"] "Email"
                th_ [scope_ "col", class_ "px-6 py-3 text-left text-xs font-medium uppercase tracking-wider"] "Role"
                th_ [scope_ "col", class_ "relative px-6 py-3"] $
                  span_ [class_ "sr-only"] "Edit"
            tbody_ [class_ ""] $! do
              forM_ users displayUser

displayUser :: User -> FloraHTML
displayUser User{username, email, userFlags} = do
  tr_ $! do
    td_ [class_ "px-6 py-4 whitespace-nowrap text-sm font-medium"] (text username)
    td_ [class_ "px-6 py-4 whitespace-nowrap text-sm font-medium"] (text email)
    td_ [class_ "px-6 py-4 whitespace-nowrap text-sm font-medium"] $! if userFlags.isAdmin then "Admin" else "User"

showUser :: User -> FloraHTML
showUser _user = undefined
