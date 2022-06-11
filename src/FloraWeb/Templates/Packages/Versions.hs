module FloraWeb.Templates.Packages.Versions where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Flora.Model.Package
import Flora.Model.Release
import qualified FloraWeb.Components.VersionListHeader as Template
import qualified FloraWeb.Components.VersionListItem as Template
import FloraWeb.Templates
import Lucid
import Optics.Core

listVersions :: Namespace -> PackageName -> Vector Release -> FloraHTML
listVersions namespace packageName releases =
  div_ [class_ "container dark:text-gray-100 text-black"] $ do
    Template.presentationHeader namespace packageName (fromIntegral $ Vector.length releases)
    div_ [class_ "md:col-span-3"] $ versionListing namespace packageName releases

versionListing :: Namespace -> PackageName -> Vector Release -> FloraHTML
versionListing namespace packageName releases = do
  ul_ [class_ "package-list space-y-2"] $ do
    Vector.forM_ releases $ \release -> do
      Template.versionListItem namespace packageName (release ^. #version)
