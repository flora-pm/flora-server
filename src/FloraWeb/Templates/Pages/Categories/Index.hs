module FloraWeb.Templates.Pages.Categories.Index where

import Data.Vector (Vector)
import Data.Vector qualified as V
import Flora.Model.Category (Category (..))
import FloraWeb.Templates (FloraHTML)
import Lucid

import FloraWeb.Components.CategoryCard (categoryCard)

index :: Vector Category -> FloraHTML
index categories =
  div_ [class_ "container"] $ do
    div_ [class_ "page-title"] $ do
      div_ [class_ "divider"] $ do
        h1_ [class_ ""] "Categories"
    div_ [class_ "larger-container categories-body grid grid-cols-1 md:grid-cols-3 gap-4 md:gap-8 my-12"] $ do
      V.forM_ categories categoryCard
