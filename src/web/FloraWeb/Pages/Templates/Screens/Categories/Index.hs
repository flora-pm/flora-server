module FloraWeb.Pages.Templates.Screens.Categories.Index where

import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as V
import Lucid

import Flora.Model.Category (Category (..))
import FloraWeb.Components.CategoryCard (categoryCard)
import FloraWeb.Pages.Templates (FloraHTML)

index :: Word -> Vector Category -> FloraHTML
index packageCount categories = do
  header_ [class_ "pageHead"] $ do
    div_ [class_ "wrapper aside"] $ do
      div_ [class_ "flow"] $ do
        h1_ [class_ "pageHead-title"] "Browse packages"
        p_ [class_ "pageHead-subtitle"] "Exploring, looking for something or just having a lazy afternoon? We got you covered!"
      p_ [class_ "title-3 self-center"] (toHtml $ display packageCount <> " packages")
  section_ [class_ "wrapper inset-large flow flow--large", id_ "content"] $ do
    h2_ [class_ "title-2"] "Categories"
    ul_ [class_ "grid grid-3 grid--large", role_ "list"] $ do
      V.forM_ categories $ \cat ->
        li_ [class_ "flex"] $
          categoryCard cat
