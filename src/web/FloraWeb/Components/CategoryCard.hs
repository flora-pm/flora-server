module FloraWeb.Components.CategoryCard where

import Lucid

import Flora.Model.Category (Category (..))
import FloraWeb.Pages.Templates (FloraHTML)

categoryCard :: Category -> FloraHTML
categoryCard Category{name, slug, synopsis} =
  a_ [class_ "entityCard ", href_ ("/categories/" <> slug)] $ do
    div_ [class_ "entityCard-title"] (toHtml name)
    div_ $
      p_ [class_ "entityCard-synopsis"] (toHtml synopsis)
