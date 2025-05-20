module FloraWeb.Pages.Templates.Screens.Admin.Groups where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Lucid

import Flora.Model.Package
import Flora.Model.PackageGroup.Types
import FloraWeb.Components.PackageGroup
import FloraWeb.Pages.Templates

index :: Vector PackageGroup -> FloraHTML
index groups = do
  newGroupForm
  table_ [class_ "package-group-list"] $ do
    thead_ [] $
      tr_ [] $ do
        th_ [] $ span_ [] "Group"
        th_ [] $ span_ [] "Actions"
    tbody_ [] $
      Vector.forM_ groups $ \group ->
        groupListItem group

newGroupForm :: FloraHTML
newGroupForm =
  div_ [class_ "new-group-form divider"] $
    form_ [action_ "/admin/groups/new", method_ "POST"] $ do
      label_ [for_ "name"] "Group name"
      input_ [type_ "text", name_ "name", required_ "", class_ "new-group-input"]
      button_ [] "Create group"

showGroup :: PackageGroup -> Vector Package -> FloraHTML
showGroup packageGroup packages = do
  packageGroupHeader packageGroup packages
