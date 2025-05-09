module FloraWeb.Components.PackageGroup where

import Lucid

import Flora.Model.PackageGroup.Types
import FloraWeb.Pages.Templates

groupCard :: PackageGroup -> FloraHTML
groupCard PackageGroup{groupName} =
  a_ [class_ "category-card", href_ ("/admin/groups/" <> groupName)] $ do
    h2_ [class_ ""] (toHtml groupName)
