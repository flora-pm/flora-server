module FloraWeb.Components.CategoryCard where

import Flora.Model.Category (Category(..))
import FloraWeb.Templates (FloraHTML)
import Lucid

categoryCard :: Category -> FloraHTML
categoryCard Category{name, slug, synopsis} = do
  a_ [class_ "card", href_ ("/categories/" <> slug)] $ do
    h2_ [class_ "font-semibold text-brand-purple dark:text-brand-purple-light"] (toHtml name)
    p_ [class_ "mt-2 text-neutral-900 dark:text-gray-200"] $ toHtml synopsis
