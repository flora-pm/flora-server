module FloraWeb.Templates.Packages.Versions where

import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Lucid

import Data.Time (defaultTimeLocale)
import Data.Time qualified as Time
import Flora.Model.Package
import Flora.Model.Release.Types
import FloraWeb.Components.PackageListItem (licenseIcon)
import FloraWeb.Components.VersionListHeader qualified as Template
import FloraWeb.Templates

listVersions :: Namespace -> PackageName -> Vector Release -> FloraHTML
listVersions namespace packageName releases =
  div_ [class_ "container"] $ do
    Template.presentationHeader namespace packageName (fromIntegral $ Vector.length releases)
    div_ [class_ "md:col-span-3"] $ do
      ul_ [class_ "package-list space-y-2"] $ do
        Vector.forM_ releases $ \release -> do
          versionListItem namespace packageName release

versionListItem :: Namespace -> PackageName -> Release -> FloraHTML
versionListItem namespace packageName release = do
  let href = href_ ("/packages/" <> display namespace <> "/" <> display packageName <> "/" <> display release.version)
  let uploadedAt = case release.uploadedAt of
        Nothing -> ""
        Just ts ->
          span_ [class_ "package-list-item__synopsis"] (toHtml $ Time.formatTime defaultTimeLocale "%a, %_d %b %Y" ts)
  li_ [class_ "package-list-item"] $
    a_ [href, class_ ""] $ do
      h4_ [class_ "package-list-item__name"] $
        strong_ [class_ ""] . toHtml $
          ("v" <> toHtml release.version)
      uploadedAt
      div_ [class_ "package-list-item__metadata"] $ do
        span_ [class_ "package-list-item__license"] $ do
          licenseIcon
          toHtml release.metadata.license
