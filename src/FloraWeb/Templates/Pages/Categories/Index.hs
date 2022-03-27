module FloraWeb.Templates.Pages.Categories.Index where

import Data.Vector (Vector)
import qualified Data.Vector as V

import Flora.Model.Category (Category (..))
import FloraWeb.Templates (FloraHTML)
import Lucid

index :: Vector Category -> FloraHTML
index categories = do
  h1_ [class_ "text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] "Categories"
  div_ [class_ "categories grid grid-cols-3 gap-3 mt-8 mb-8"] $ do
    V.forM_ categories categoryCard

categoryCard :: Category -> FloraHTML
categoryCard Category{name, slug, synopsis} = do
  div_ [class_ "category-card max-w-md py-4 px-8 shadow-lg rounded-lg my-16"] $ do
    div_ $ do
      h2_ [class_ "font-semibold"] $
        a_ [class_ "category-name", href_ ("/categories/" <> slug)] (toHtml name)
      p_ [class_ "mt-2 text-gray-200"] $ toHtml synopsis
