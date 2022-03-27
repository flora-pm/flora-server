module FloraWeb.Templates.Packages.Listing where
import Data.Text (Text)
import Data.Text.Display (display)
import Distribution.Types.Version (Version)
import FloraWeb.Templates (FloraHTML)
import Lucid

import Flora.Model.Package
import Flora.Model.Release.Orphans ()

showPackage :: (Namespace, PackageName, Text, Version) -> FloraHTML
showPackage (namespace, name, synopsis, version) = do
  a_ [href_ ("/packages/@" <> display namespace <> "/" <> display name)] $ do
    div_ [class_ "text-slate-300 hover:text-slate-200"] $ do
      p_ [class_ "package-name inline text-link dark:text-link-dark"]
          (toHtml $ prettyPackageName namespace name)
      p_ [class_ "synopsis inline ml-3"] (toHtml synopsis)
    div_ [class_ "text-slate-300 text-sm"] $ "v" <> toHtml (display version)

prettyPackageName :: Namespace -> PackageName -> Text
prettyPackageName namespace name = "@" <> display namespace <> "/" <> display name
