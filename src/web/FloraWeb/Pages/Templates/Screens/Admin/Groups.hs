module FloraWeb.Pages.Templates.Screens.Admin.Groups where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Lucid

import Flora.Model.Package.Types
import Flora.Model.PackageGroup.Types
import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Components.PackageGroup
import FloraWeb.Pages.Templates

index :: Vector PackageGroup -> FloraHTML
index groups = do
  header_ [class_ "pageHead"] $ do
    div_ [class_ "wrapper aside"] $ do
      h1_ [class_ "pageHead-title"] "Packages Group"
      div_ [class_ "self-center"] $ do
        a_ [href_ "/admin/"] $ do
          Icons.arrowLeft
          "Back to Overview"
  div_ [class_ "wrapper inset-region flow flow--large"] $ do
    newGroupForm
    table_ [class_ ""] $ do
      thead_ [] $
        tr_ [] $ do
          th_ [] $ span_ [] "Group"
          th_ [] $ span_ [] "Actions"
      tbody_ [] $
        Vector.forM_ groups $ \group ->
          groupListItem group

newGroupForm :: FloraHTML
newGroupForm =
  div_ [class_ ""] $
    form_ [action_ "/admin/groups/new", method_ "POST", class_ "cluster"] $ do
      label_ [for_ "name"] "Group name"
      input_ [type_ "text", name_ "name", required_ "", class_ "new-group-input"]
      button_ [class_ "btn"] "Create group"

showGroup :: PackageGroup -> Vector PackageInfo -> FloraHTML
showGroup packageGroup packageInfo = do
  packageGroupHeader packageGroup packageInfo
  addPackageToGroupForm packageGroup.packageGroupId
  table_ [class_ "group-packages-list"] $ do
    thead_ [] $
      tr_ [] $ do
        th_ [] $ span_ [] "Package"
        th_ [] $ span_ [] "Actions"
    tbody_ [] $
      Vector.forM_ packageInfo $ \package ->
        groupPackageListItem packageGroup.packageGroupId package
