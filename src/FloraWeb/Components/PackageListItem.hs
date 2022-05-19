module FloraWeb.Components.PackageListItem
  ( packageListItem
  ) where

import Data.Text (Text)
import FloraWeb.Templates (FloraHTML)
import Lucid
import Data.Text.Display (display)

import Flora.Model.Package (Namespace, PackageName)

packageListItem :: (Namespace, PackageName, Text, Text) -> FloraHTML
packageListItem (namespace, name, synopsis, version) = do
  let href = href_ ("/packages/@" <> display namespace <> "/" <> display name)
  li_ [class_ "packages-list-item xl:text-xl dark:text-gray-200"] $
    a_ [href, class_ "block text-inherit md:my-6" ] $ do
      h4_ [class_ "package-name inline mr-2"] $
        strong_ [] . toHtml $ prettyPackageName namespace name
      p_ [class_ "inline text-neutral-900 dark:text-gray-200"] $ toHtml synopsis
      div_ [class_ "text-slate-300 text-sm mt-2"] $ toHtml version

prettyPackageName :: Namespace -> PackageName -> Text
prettyPackageName namespace name = "@" <> display namespace <> "/" <> display name
