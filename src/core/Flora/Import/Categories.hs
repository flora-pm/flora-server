module Flora.Import.Categories where

import Control.Monad.IO.Class
import Data.Text.IO qualified as T
import Effectful
import Effectful.PostgreSQL.Transact.Effect

import Data.Text (Text)
import Flora.Model.Category.Types (Category, mkCategory, mkCategoryId)
import Flora.Model.Category.Update (insertCategory)
import Flora.Normalise

importCategories :: (DB :> es, IOE :> es) => Eff es ()
importCategories = do
  liftIO $ T.putStrLn "Sourcing categories"
  categories <- mapM fromCanonical floraCategories
  mapM_ insertCategory categories

fromCanonical :: IOE :> es => (Text, Text, Text) -> Eff es Category
fromCanonical (slug, name, synopsis) = do
  categoryId <- liftIO mkCategoryId
  pure $ mkCategory categoryId name (Just slug) synopsis
