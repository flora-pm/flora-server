module FloraWeb.Components.PackageGroup where

import Data.Text (Text)
import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Htmx.Lucid.Extra
import Lucid
import Web.HttpApiData

import Flora.Model.Package
import Flora.Model.PackageGroup.Types
import FloraWeb.Components.Icons qualified as Icon
import FloraWeb.Components.Utils
import FloraWeb.Pages.Templates

groupListItem :: PackageGroup -> FloraHTML
groupListItem PackageGroup{packageGroupId, groupName} =
  tr_ [class_ "package-group divider"] $ do
    td_ [] $
      a_ [class_ "", href_ ("/admin/groups/" <> toUrlPiece packageGroupId)] $ do
        span_ [class_ ""] (toHtml groupName)
    td_ [class_ "group-table-actions"]
      $ button_
        [class_ "delete-group", hxDelete_ ("/admin/groups/delete/" <> toUrlPiece packageGroupId), ariaLabel_ "Delete"]
      $ i_ [class_ "fa-solid fa-trash"] mempty

packageGroupHeader :: PackageGroup -> Vector Package -> FloraHTML
packageGroupHeader packageGroup packages = div_ [class_ "divider"] $ do
  div_ [class_ "page-title"] $ h1_ [class_ ""] $ do
    span_ [class_ "headline"] $ do
      toHtml @Text "Package Groups"
      Icon.chevronRightOutline
      toHtml packageGroup.groupName
  p_ [class_ "synopsis"] $
    span_ [class_ "version"] $
      toHtml $
        display (Vector.length packages)
          <> " packages"
