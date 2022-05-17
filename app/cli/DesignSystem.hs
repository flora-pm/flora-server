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

newtype ComponentName = ComponentName String
  deriving newtype (Eq, Ord, Show)

generateComponents :: IO ()
generateComponents = forM_ components $ \(name, template) -> do
  let html =  renderHtml template
  writeComponent name html
  
renderHtml :: FloraHTML -> ByteString
renderHtml template =
  runIdentity $ runReaderT (renderBST template) templateEnv
    where
      templateEnv = defaultsToEnv defaultTemplateEnv

writeComponent :: ComponentName -> ByteString -> IO ()
writeComponent componentName html =
  ByteString.writeFile ("./assets/design/components/" <> show componentName <> ".html") html

components :: Vector (ComponentName, FloraHTML)
components = Vector.fromList
  [ (ComponentName "package-list-item", packageListItemExample)
  , (ComponentName "category-card", categoryCardExample)
  ]

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
