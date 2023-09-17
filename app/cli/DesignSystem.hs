{-# LANGUAGE QuasiQuotes #-}

module DesignSystem where

import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString
import Data.Foldable (forM_)
import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Version
import Effectful
import Effectful.Fail
import Env
import Lucid
import PyF (fmt)

import Distribution.SPDX
import Flora.Environment.Config
import Flora.Model.Category
import Flora.Model.Category qualified as Category
import Flora.Model.Package
import Flora.Search
import FloraWeb.Components.CategoryCard qualified as Component
import FloraWeb.Components.PackageListItem qualified as Component
import FloraWeb.Components.PaginationNav qualified as Component
import FloraWeb.Pages.Templates.Types

newtype ComponentName = ComponentName Text
  deriving newtype (Eq, Ord, Show)

newtype ComponentTitle = ComponentTitle Text
  deriving newtype (Eq, Ord, Show)

generateComponents :: (Fail :> es, IOE :> es) => Eff es ()
generateComponents = do
  environment <- liftIO $ Env.parse id parseDeploymentEnv
  assets <- getAssets environment
  forM_ components $ \(filename, title, name, template) -> do
    let html = TL.replace "\"" "\\\"" $ renderHtml assets template
    writeComponent filename title name html

renderHtml :: Assets -> FloraHTML -> TL.Text
renderHtml assets template =
  runIdentity $ runReaderT (renderTextT template) templateEnv
  where
    templateEnv = defaultsToEnv assets defaultTemplateEnv

writeComponent :: IOE :> es => FilePath -> ComponentTitle -> ComponentName -> TL.Text -> Eff es ()
writeComponent filename title name html =
  liftIO $
    ByteString.writeFile
      ("./design/stories/" <> filename <> ".stories.js")
      (storyTemplate title name html)

-- | A component is represented by a 4-tuple of
components :: Vector (FilePath, ComponentTitle, ComponentName, FloraHTML)
components =
  Vector.fromList
    [
      ( "package-list--item"
      , ComponentTitle "Package List"
      , ComponentName "PackageListItem"
      , packageListItemExample
      )
    , ("category-card", ComponentTitle "Category", ComponentName "CategoryCard", categoryCardExample)
    , ("pagination-area", ComponentTitle "Pagination Area", ComponentName "Pagination", paginationExample)
    ]

-----------------------
-- Storybook Helpers --
-----------------------

storyTemplate :: ComponentTitle -> ComponentName -> TL.Text -> ByteString
storyTemplate (ComponentTitle title) (ComponentName name) html =
  [fmt| 
export default {{
  title: "Components/{title}"
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
    , "Basic libraries"
    , mkVersion [4, 16, 0, 0]
    , License (simpleLicenseExpression BSD_3_Clause)
    )

categoryCardExample :: FloraHTML
categoryCardExample =
  Component.categoryCard categoryExample

categoryExample :: Category
categoryExample =
  Category.mkCategory (CategoryId UUID.nil) "Prelude" Nothing "Libraries that provide default imports"

namespaceExample :: Namespace
namespaceExample = Namespace "haskell"

packageNameExample :: PackageName
packageNameExample = PackageName "base"

paginationExample :: FloraHTML
paginationExample = div_ $ do
  div_ $ do
    h4_ "Two buttons"
    Component.paginationNav 62 2 (SearchPackages "text")
  div_ $ do
    h4_ "Previous button"
    Component.paginationNav 32 3 (SearchPackages "text")
  div_ $ do
    h4_ "Next button"
    Component.paginationNav 32 1 (SearchPackages "text")
