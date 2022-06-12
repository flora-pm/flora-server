module FloraWeb.Components.VersionListItem
  ( versionListItem
  )
where

import Data.Text.Display (display)
import Distribution.Types.Version (Version)
import Flora.Model.Package
import Flora.Model.Release.Orphans ()
import FloraWeb.Components.Utils (text)
import FloraWeb.Templates
import Lucid

versionListItem :: Namespace -> PackageName -> Version -> FloraHTML
versionListItem namespace packageName version = do
  let href = href_ ("/packages/" <> display namespace <> "/" <> display packageName <> "/" <> display version)
  li_ [class_ "version-list-item"] $
    a_ [href, class_ "block md:my-4"] $ do
      h4_ [class_ "version-list-item__name"] $
        strong_ [class_ ""] $
          text ("v" <> display version)
