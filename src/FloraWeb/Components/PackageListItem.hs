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
  li_ [class_ "package-list__item xl:text-xl dark:text-gray-200"] $
    a_ [href, class_ "block md:my-6"] $ do
      h4_ [class_ "inline mr-2"] $
        strong_ [class_ "package-list__item__name"] . toHtml $ prettyPackageName namespace name
      p_ [class_ "inline text-neutral-900 dark:text-gray-200"] $ toHtml synopsis
      div_ [class_ "text-slate-300 text-sm mt-2"] $ toHtml version

prettyPackageName :: Namespace -> PackageName -> Text
prettyPackageName namespace name = "@" <> display namespace <> "/" <> display name
