module FloraWeb.Components.PackageListItem
  ( packageListItem
  )
where

import Data.Text (Text)
import Data.Text.Display (display)
import FloraWeb.Templates (FloraHTML)
import Lucid

import Flora.Model.Package (Namespace, PackageName)

packageListItem :: (Namespace, PackageName, Text, Text) -> FloraHTML
packageListItem (namespace, name, synopsis, version) = do
  let href = href_ ("/packages/@" <> display namespace <> "/" <> display name)
  li_ [class_ "package-list-item xl:text-xl"] $
    a_ [href, class_ "block md:my-6"] $ do
      h4_ [class_ "package-list-item__name"] $
        strong_ [class_ ""] . toHtml $ prettyPackageName namespace name
      p_ [class_ "package-list-item__synopsis"] $ toHtml synopsis
      div_ [class_ "package-list-item__version text-sm mt-2"] $ toHtml version

prettyPackageName :: Namespace -> PackageName -> Text
prettyPackageName namespace name = "@" <> display namespace <> "/" <> display name
