module FloraWeb.Components.PackageListItem
  ( packageListItem
  , packageListItemWithVersionRange
  , licenseIcon
  )
where

import Data.Text (Text)
import Data.Text.Display (display)
import FloraWeb.Templates (FloraHTML)
import Lucid

import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.Version (Version)
import Flora.Model.Package (Namespace, PackageName)
import Lucid.Orphans ()
import Lucid.Svg (clip_rule_, d_, fill_, fill_rule_, path_, viewBox_)

packageListItem :: (Namespace, PackageName, Text, Version, SPDX.License) -> FloraHTML
packageListItem (namespace, packageName, synopsis, version, license) = do
  let href = href_ ("/packages/" <> display namespace <> "/" <> display packageName)
  li_ [class_ "package-list-item"] $
    a_ [href, class_ ""] $ do
      h4_ [class_ "package-list-item__name"] $
        strong_ [class_ ""] . toHtml $
          display namespace <> "/" <> display packageName
      p_ [class_ "package-list-item__synopsis"] $ toHtml synopsis
      div_ [class_ "package-list-item__metadata"] $ do
        span_ [class_ "package-list-item__license"] $ do
          licenseIcon
          toHtml license
        span_ [class_ "package-list-item__version"] $ "v" <> toHtml version

packageListItemWithVersionRange :: (Namespace, PackageName, Text, Text, SPDX.License) -> FloraHTML
packageListItemWithVersionRange (namespace, packageName, synopsis, versionRange, license) = do
  let href = href_ ("/packages/" <> display namespace <> "/" <> display packageName)
  li_ [class_ "package-list-item"] $
    a_ [href, class_ ""] $ do
      h4_ [class_ "package-list-item__name"] $
        strong_ [class_ ""] . toHtml $
          display namespace <> "/" <> display packageName
      p_ [class_ "package-list-item__synopsis"] $ toHtml synopsis
      div_ [class_ "package-list-item__metadata"] $ do
        span_ [class_ "package-list-item__license"] $ do
          licenseIcon
          toHtml license
        displayVersionRange versionRange

displayVersionRange :: Text -> FloraHTML
displayVersionRange versionRange =
  if versionRange == ">=0"
    then ""
    else span_ [class_ "package-list-item__version-range"] $ toHtml versionRange

licenseIcon :: FloraHTML
licenseIcon =
  svg_ [xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 20 20", fill_ "currentColor", class_ "license-icon"] $
    path_ [fill_rule_ "evenodd", d_ "M10 2a.75.75 0 01.75.75v.258a33.186 33.186 0 016.668.83.75.75 0 01-.336 1.461 31.28 31.28 0 00-1.103-.232l1.702 7.545a.75.75 0 01-.387.832A4.981 4.981 0 0115 14c-.825 0-1.606-.2-2.294-.556a.75.75 0 01-.387-.832l1.77-7.849a31.743 31.743 0 00-3.339-.254v11.505a20.01 20.01 0 013.78.501.75.75 0 11-.339 1.462A18.558 18.558 0 0010 17.5c-1.442 0-2.845.165-4.191.477a.75.75 0 01-.338-1.462 20.01 20.01 0 013.779-.501V4.509c-1.129.026-2.243.112-3.34.254l1.771 7.85a.75.75 0 01-.387.83A4.98 4.98 0 015 14a4.98 4.98 0 01-2.294-.556.75.75 0 01-.387-.832L4.02 5.067c-.37.07-.738.148-1.103.232a.75.75 0 01-.336-1.462 32.845 32.845 0 016.668-.829V2.75A.75.75 0 0110 2zM5 7.543L3.92 12.33a3.499 3.499 0 002.16 0L5 7.543zm10 0l-1.08 4.787a3.498 3.498 0 002.16 0L15 7.543z", clip_rule_ "evenodd"]
