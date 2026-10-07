module Flora.Domain.Import.Categories where

import Data.Text (Text)
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Log

import Flora.Database
import Flora.Domain.Category.Normalise
import Flora.Environment.Env
import Flora.Model.Category.Types (Category, deterministicCategoryId, mkCategory)
import Flora.Model.Category.Update (bulkInsertCategories)
import Flora.Monad

importCategories :: (IOE :> es, Reader FloraEnv :> es) => FloraM es ()
importCategories = do
  FloraEnv{pool} <- Reader.ask
  Log.logInfo_ "Sourcing categories"
  withReadWritePool pool $
    bulkInsertCategories (fmap fromCanonical floraCategories)
  Log.logInfo_ "Categories done sourcing"

fromCanonical :: (Text, Text, Text) -> Category
fromCanonical (slug, name, synopsis) =
  mkCategory (deterministicCategoryId slug) name (Just slug) synopsis
