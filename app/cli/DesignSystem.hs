{-# LANGUAGE QuasiQuotes #-}
module DesignSystem where

import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (forM_)
import Data.Functor.Identity (runIdentity)
import Data.Vector (Vector)
import Lucid.Base
import qualified Data.ByteString.Lazy as ByteString
import Data.ByteString.Lazy (ByteString)
import qualified Data.Vector as Vector
import qualified Data.UUID as UUID

import qualified FloraWeb.Components.PackageListItem as Component
import qualified FloraWeb.Components.CategoryCard as Component
import FloraWeb.Templates (FloraHTML, defaultTemplateEnv, defaultsToEnv)
import Flora.Model.Package
import Flora.Model.Category
import qualified Flora.Model.Category as Category
import PyF (fmt)

newtype ComponentName = ComponentName String
  deriving newtype (Eq, Ord, Show)

generateComponents :: IO ()
generateComponents = forM_ components $ \(filename, name, template) -> do
  let html =  renderHtml template
  writeComponent filename name html
  
renderHtml :: FloraHTML -> ByteString
renderHtml template =
  runIdentity $ runReaderT (renderBST template) templateEnv
    where
      templateEnv = defaultsToEnv defaultTemplateEnv

writeComponent :: FilePath -> ComponentName -> ByteString -> IO ()
writeComponent filename name html =
  ByteString.writeFile ("./design/stories/" <> filename <> ".stories.js") 
                       (storyTemplate name html)

-- | A component is represented by a 4-tuple of 
components :: Vector (FilePath, ComponentName, FloraHTML)
components = Vector.fromList
  [ ("package-list--item", ComponentName "Package List/Item", packageListItemExample)
  , ("category-card", ComponentName "Category/Card", categoryCardExample)
  ]

-----------------------
-- Storybook Helpers --
-----------------------

storyTemplate :: ComponentName -> ByteString -> ByteString
storyTemplate (ComponentName name) html = [fmt| 
    export default {{
      title: 'Example/Headings'
    }};

    export const {name} = () => {html}
    |]

--------------
-- Examples --
--------------

packageListItemExample :: FloraHTML
packageListItemExample =
  Component.packageListItem
    ( namespaceExample
    , packageNameExample
    , "Basic libraries", "4.16.0.0"
    )

categoryCardExample :: FloraHTML
categoryCardExample =
  Component.categoryCard categoryExample

categoryExample :: Category
categoryExample = Category.mkCategory (CategoryId UUID.nil) "Prelude" Nothing "Libraries that provide default imports"

namespaceExample :: Namespace
namespaceExample = Namespace "haskell"

packageNameExample :: PackageName
packageNameExample = PackageName "base"
