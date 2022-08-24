module FloraWeb.Templates.Packages.Versions where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Lucid

import Flora.Model.Package
import Flora.Model.Release.Types
import FloraWeb.Components.VersionListHeader qualified as Template
import FloraWeb.Components.VersionListItem qualified as Template
import FloraWeb.Templates

listVersions :: Namespace -> PackageName -> Vector Release -> FloraHTML
listVersions namespace packageName releases =
  div_ [class_ "container dark:text-gray-100 text-black"] $ do
    Template.presentationHeader namespace packageName (fromIntegral $ Vector.length releases)
    div_ [class_ "md:col-span-3"] $ versionListing namespace packageName releases

versionListing :: Namespace -> PackageName -> Vector Release -> FloraHTML
versionListing namespace packageName releases = do
  ul_ [class_ "package-list space-y-2"] $ do
    Vector.forM_ releases $ \release -> do
      Template.versionListItem namespace packageName (release.version)
