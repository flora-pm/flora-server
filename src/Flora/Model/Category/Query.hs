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
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple.SqlQQ (sql)
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
      query Select [sql|
        select  p.namespace
              , p.name
              , p.synopsis
              , p.metadata
              , p.owner_id
              , p.created_at
              , p.updated_at
        from packages as p
        inner join package_categories as pc on (p.namespace = pc.package_namespace and p.name = pc.package_name)
        where pc.category_id = ?
        |] (Only categoryId)

getAllCategories :: (MonadIO m) => DBT m (Vector Category)
getAllCategories = query_ Select (_select @Category)
