module FloraWeb.Pages.Templates.Screens.Admin.Groups where

import Data.Vector (Vector)
import Data.Vector qualified as Vector

import Flora.Model.PackageGroup.Types
import FloraWeb.Components.PackageGroup
import FloraWeb.Pages.Templates

index :: Vector PackageGroup -> FloraHTML
index groups = do
  Vector.forM_ groups $ \group ->
    groupCard group
