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
  let classes = "block text-inherit my-4 md:my-6"
  a_ [href, class_ classes] $ do
    h4_ [class_ "package-name inline text-link dark:text-link-dark mr-2"] $
      strong_ [] . toHtml $ prettyPackageName namespace name
    p_ [class_ "inline text-neutral-900 dark:text-gray-200"] $ toHtml synopsis
    div_ [class_ "text-slate-300 text-sm"] $ toHtml version

prettyPackageName :: Namespace -> PackageName -> Text
prettyPackageName namespace name = "@" <> display namespace <> "/" <> display name
