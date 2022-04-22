module FloraWeb.Templates.Pages.Categories.Index where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Flora.Model.Category (Category (..))
import FloraWeb.Templates (FloraHTML)
import Lucid

index :: Vector Category -> FloraHTML
index categories = do
  h1_ [class_ "text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] "Categories"
  div_ [class_ "narrow-container grid grid-cols-1 md:grid-cols-3 gap-4 md:gap-8 my-12"] $ do
    V.forM_ categories categoryCard

categoryCard :: Category -> FloraHTML
categoryCard Category {name, slug, synopsis} = do
  a_ [class_ "card", href_ ("/categories/" <> slug)] $ do
    h2_ [class_ "font-semibold text-brand-purple dark:text-brand-purple-light"] (toHtml name)
    p_ [class_ "mt-2 text-neutral-900 dark:text-gray-200"] $ toHtml synopsis
