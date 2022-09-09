module FloraWeb.Components.VersionListHeader where

import Data.Text (Text)
import Data.Text.Display (display)
import Distribution.Orphans ()
import Flora.Model.Package (Namespace, PackageName, formatPackage)
import FloraWeb.Components.Utils (text)
import FloraWeb.Templates.Types
import Lucid

presentationHeader ::
  Namespace ->
  PackageName ->
  Word ->
  FloraHTML
presentationHeader namespace packageName numberOfReleases = do
  div_ [class_ "divider"] $ do
    div_ [class_ "px-4 py-5 sm:px-6 sm:py-24 lg:py-4 lg:px-8"] $
      h2_ [class_ "text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] $ do
        span_ [class_ "headline"] $ text ("All " <> display numberOfReleases <> " versions of " <> formatPackage namespace packageName)
        toHtmlRaw @Text "&nbsp;"
