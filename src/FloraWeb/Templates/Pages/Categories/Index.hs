module FloraWeb.Templates.Pages.Categories.Index where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Flora.Model.Category (Category (..))
import FloraWeb.Templates (FloraHTML)
import Lucid

import FloraWeb.Components.CategoryCard (categoryCard)

index :: Vector Category -> FloraHTML
index categories = do
  h1_ [class_ "mt-10 text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] "Categories"
  div_ [class_ "larger-container grid grid-cols-1 md:grid-cols-3 gap-4 md:gap-8 my-12"] $ do
    V.forM_ categories categoryCard

