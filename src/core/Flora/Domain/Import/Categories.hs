module Flora.Domain.Import.Categories where

import Control.Monad.IO.Class
import Data.Text (Text)
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Log

import Flora.Database
import Flora.Domain.Category.Normalise
import Flora.Environment.Env
import Flora.Model.Category.Types (Category, mkCategory, mkCategoryId)
import Flora.Model.Category.Update (bulkInsertCategories)
import Flora.Monad

importCategories :: (IOE :> es, Reader FloraEnv :> es) => FloraM es ()
importCategories = do
  FloraEnv{pool} <- Reader.ask
  Log.logInfo_ "Sourcing categories"
  categories <- mapM fromCanonical floraCategories
  withReadWritePool pool $
    bulkInsertCategories categories
  Log.logInfo_ "Categories done sourcing"

fromCanonical :: IOE :> es => (Text, Text, Text) -> Eff es Category
fromCanonical (slug, name, synopsis) = do
  categoryId <- liftIO mkCategoryId
  pure $ mkCategory categoryId name (Just slug) synopsis
