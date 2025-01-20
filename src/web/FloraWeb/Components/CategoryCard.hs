module FloraWeb.Components.CategoryCard where

import Lucid

import Flora.Model.Category (Category (..))
import FloraWeb.Pages.Templates (FloraHTML)

categoryCard :: Category -> FloraHTML
categoryCard Category{name, slug, synopsis} =
  a_ [class_ "category-card", href_ ("/categories/" <> slug)] $ do
    h2_ [class_ "category-card__name font-semibold text-purple"] (toHtml name)
    p_ [class_ "category-card__synopsis mt-2"] $ toHtml synopsis
