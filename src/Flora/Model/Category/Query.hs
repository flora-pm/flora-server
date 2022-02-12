{-# LANGUAGE QuasiQuotes #-}
module Flora.Model.Category.Query where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Transact (DBT)

import qualified Data.Text.IO as T
import Database.PostgreSQL.Entity
import Flora.Model.Category.Types
import Flora.Model.Package.Types

getCategoryById :: (MonadIO m) => CategoryId -> DBT m (Maybe Category)
getCategoryById categoryId = selectById (Only categoryId)

getCategoryBySlug :: (MonadIO m) => Text -> DBT m (Maybe Category)
getCategoryBySlug slug = selectOneByField [field| slug |] (Only slug)

getCategoryByName :: (MonadIO m) => Text -> DBT m (Maybe Category)
getCategoryByName categoryName = selectOneByField [field| name |] (Only categoryName)

getPackagesFromCategorySlug :: (MonadIO m) => Text -> DBT m (Vector Package)
getPackagesFromCategorySlug slug = do
  getCategoryBySlug slug
  >>= \case
    Nothing -> do
      liftIO $ T.putStrLn $ "Could not find category from slug: \"" <> slug <> "\""
      pure Vector.empty
    Just Category{categoryId} -> do
      liftIO $ T.putStrLn "Category found!"
      joinSelectOneByField @Package @PackageCategory [field| package_id |] [field| category_id |] categoryId
