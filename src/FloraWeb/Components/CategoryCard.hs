module FloraWeb.Components.CategoryCard where

import Flora.Model.Category (Category (..))
import FloraWeb.Templates (FloraHTML)
import Lucid

categoryCard :: Category -> FloraHTML
categoryCard Category{name, slug, synopsis} =
  div_ [class_ ""] $
    a_ [class_ "category-card", href_ ("/categories/" <> slug)] $ do
      h2_ [class_ "category-card__name font-semibold text-purple"] (toHtml name)
      p_ [class_ "category-card__synopsis mt-2"] $ toHtml synopsis
