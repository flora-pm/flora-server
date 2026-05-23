module Flora.Import.Categories where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text.IO qualified as T
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader

import Flora.Database
import Flora.Environment.Env
import Flora.Model.Category.Types (Category, mkCategory, mkCategoryId)
import Flora.Model.Category.Update (insertCategory)
import Flora.Normalise

importCategories :: (IOE :> es, Reader FloraEnv :> es) => Eff es ()
importCategories = do
  FloraEnv{pool} <- Reader.ask
  liftIO $ T.putStrLn "Sourcing categories"
  categories <- mapM fromCanonical floraCategories
  withReadWritePool pool $
    mapM_ insertCategory categories

fromCanonical :: IOE :> es => (Text, Text, Text) -> Eff es Category
fromCanonical (slug, name, synopsis) = do
  categoryId <- liftIO mkCategoryId
  pure $ mkCategory categoryId name (Just slug) synopsis
