module FloraWeb.Pages.Templates.Screens.Categories.Index where

import Data.Vector (Vector)
import Data.Vector qualified as V
import Lucid

import Flora.Model.Category (Category (..))
import FloraWeb.Components.CategoryCard (categoryCard)
import FloraWeb.Pages.Templates (FloraHTML)

index :: Vector Category -> FloraHTML
index categories =
  div_ [class_ "container"] $ do
    div_ [class_ "categories-title"] $ do
      div_ [class_ "divider-with-margin"] $ do
        h1_ [class_ ""] "Categories"
    div_ [class_ "categories-body"] $ do
      V.forM_ categories categoryCard
