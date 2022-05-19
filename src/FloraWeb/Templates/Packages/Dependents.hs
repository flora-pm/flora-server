module FloraWeb.Templates.Packages.Dependents where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Distribution.Types.Version (Version)
import Flora.Model.Package (Namespace, PackageName)
import FloraWeb.Templates (FloraHTML)
import FloraWeb.Templates.Packages.Listing
import Lucid

showDependents :: Text -> Vector (Namespace, PackageName, Text, Version) -> FloraHTML
showDependents searchString packagesInfo = do
  div_ [class_ "container px-2 grid md:grid-cols-4 md:gap-6"] $ do
    div_ [class_ "md:mt-6"] $ searchResultsAside (Just searchString) (fromIntegral $ Vector.length packagesInfo)
    div_ [class_ "md:col-span-3"] $ packageListing packagesInfo
