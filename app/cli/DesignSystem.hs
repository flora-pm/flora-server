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
import Data.Text (Text)
import qualified Data.Text.Lazy as TL

newtype ComponentName = ComponentName Text
  deriving newtype (Eq, Ord, Show)

newtype ComponentTitle = ComponentTitle Text
  deriving newtype (Eq, Ord, Show)

generateComponents :: IO ()
generateComponents = forM_ components $ \(filename, title, name, template) -> do
  let html =  renderHtml template
  writeComponent filename title name html
  
renderHtml :: FloraHTML -> TL.Text
renderHtml template =
  runIdentity $ runReaderT (renderTextT template) templateEnv
    where
      templateEnv = defaultsToEnv defaultTemplateEnv

writeComponent :: FilePath -> ComponentTitle -> ComponentName -> TL.Text -> IO ()
writeComponent filename title name html =
  ByteString.writeFile ("./design/stories/" <> filename <> ".stories.js") 
                       (storyTemplate title name html)

-- | A component is represented by a 4-tuple of 
components :: Vector (FilePath, ComponentTitle, ComponentName, FloraHTML)
components = Vector.fromList
  [ ("package-list--item", ComponentTitle "Package List/Item", ComponentName "PackageListItem", packageListItemExample)
  , ("category-card", ComponentTitle "Category/Card", ComponentName "CategoryCard", categoryCardExample)
  ]

-----------------------
-- Storybook Helpers --
-----------------------

storyTemplate :: ComponentTitle -> ComponentName -> TL.Text -> ByteString
storyTemplate (ComponentTitle title)(ComponentName name) html = [fmt| 
export default {{
  title: "{title}"
}};

export const {name} = () => "{html}"
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
