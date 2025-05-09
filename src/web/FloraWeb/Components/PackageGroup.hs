module FloraWeb.Components.PackageGroup where

import Htmx.Lucid.Extra
import Lucid
import Web.HttpApiData

import Flora.Model.PackageGroup.Types
import FloraWeb.Components.Utils
import FloraWeb.Pages.Templates

groupCard :: PackageGroup -> FloraHTML
groupCard PackageGroup{packageGroupId, groupName} =
  tr_ [class_ "package-group divider"] $ do
    td_ [] $
      a_ [class_ "", href_ ("/admin/groups/" <> toUrlPiece packageGroupId)] $ do
        span_ [class_ ""] (toHtml groupName)
    td_ [class_ "group-table-actions"]
      $ button_
        [class_ "delete-group", hxDelete_ ("/admin/groups/delete/" <> toUrlPiece packageGroupId), ariaLabel_ "Delete"]
      $ i_ [class_ "fa fa-trash"] mempty
