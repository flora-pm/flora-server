module FloraWeb.Components.PackageGroup where

import Data.Text (Text)
import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Lucid
import Web.HttpApiData

import Flora.Model.Package
import Flora.Model.PackageGroup.Types
import FloraWeb.Components.Button
import FloraWeb.Components.Icons qualified as Icon
import FloraWeb.Components.Utils
import FloraWeb.Links qualified as Links
import FloraWeb.Pages.Templates

linkToGroups :: FloraHTML
linkToGroups =
  a_
    [ class_ "breadcrumb-segment"
    , href_ "/admin/groups"
    ]
    (toHtml @Text $ "Package Groups")

groupListItem :: PackageGroup -> FloraHTML
groupListItem PackageGroup{packageGroupId, groupName} =
  tr_ [class_ "package-group divider"] $ do
    td_ [] $
      a_ [class_ "", href_ ("/admin/groups/" <> toUrlPiece packageGroupId)] $ do
        span_ [class_ ""] (toHtml groupName)
    td_ [class_ "group-table-actions"] $ do
      form_ [action_ ("/admin/groups/delete/" <> toUrlPiece packageGroupId), method_ "POST"]
        $ button_
          [class_ "delete-group", ariaLabel_ "Delete"]
        $ i_ [class_ "fa-solid fa-trash"] mempty

groupPackageListItem :: PackageGroupId -> PackageInfo -> FloraHTML
groupPackageListItem packageGroupId packageInfo = do
  let formattedPackageName = display packageInfo.namespace <> "/" <> display packageInfo.name
  tr_ [class_ "package-group divider"] $ do
    td_ [] $
      a_ [class_ "", href_ ("/" <> (Links.renderLink $ Links.packageLink packageInfo.namespace packageInfo.name))] $ do
        span_ [class_ ""] (toHtml @Text formattedPackageName)
    td_ [class_ "group-table-actions"] $ do
      form_ [action_ ("/admin/groups/" <> toUrlPiece packageGroupId <> "/remove/" <> toUrlPiece packageInfo.packageId), method_ "POST"]
        $ button_
          [class_ "delete-group", ariaLabel_ "Delete"]
        $ i_ [class_ "fa-solid fa-trash"] mempty

packageGroupHeader :: PackageGroup -> Vector PackageInfo -> FloraHTML
packageGroupHeader packageGroup packages = div_ [class_ "divider"] $ do
  div_ [class_ "page-title"] $ h1_ [class_ ""] $ do
    span_ [class_ "headline"] $ do
      linkToGroups
      Icon.chevronRightOutline
      toHtml packageGroup.groupName
  p_ [class_ "synopsis"] $
    span_ [class_ "version"] $
      toHtml $
        display (Vector.length packages)
          <> " packages"

addPackageToGroupForm :: PackageGroupId -> FloraHTML
addPackageToGroupForm groupId = do
  form_ [action_ ("/admin/groups/" <> toUrlPiece groupId <> "/add"), class_ "add-package-to-group-form divider", method_ "POST"] $ do
    div_ [class_ ""] $ do
      label_ [for_ "namespace"] "Namespace"
      input_
        [ class_ "new-group-input"
        , id_ "namespace"
        , type_ "text"
        , name_ "namespace"
        ]
    div_ [class_ ""] $ do
      label_ [for_ "package"] "Package"
      input_
        [ class_ "new-group-input"
        , id_ "package"
        , type_ "text"
        , name_ "package"
        ]
    button "Add package"
