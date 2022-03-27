module Flora.Model.Category.Update where

import Control.Monad.IO.Class
import Data.Text (Text)
import Database.PostgreSQL.Entity (insert)
import Database.PostgreSQL.Transact (DBT)

import qualified Data.Text.IO as T
import qualified Data.UUID.V4 as UUID
import qualified Flora.Model.Category.Query as Query
import Flora.Model.Category.Types
import Flora.Model.Package.Types

insertCategory :: (MonadIO m) => Category -> DBT m ()
insertCategory category = insert @Category category

addToCategory :: (MonadIO m) => (Namespace, PackageName) -> CategoryId -> DBT m ()
addToCategory (namespace, packageName) categoryId = do
  packageCategoryId <- PackageCategoryId <$> liftIO UUID.nextRandom
  insert @PackageCategory (packageCategoryId, namespace, packageName, categoryId)

addToCategoryByName :: (MonadIO m)  => (Namespace, PackageName) -> Text -> DBT m ()
addToCategoryByName (namespace, packageName) categoryName = do
  mCategory <- Query.getCategoryByName categoryName
  case mCategory of
    Nothing -> do
      liftIO $ T.putStrLn ("Could not find category " <> categoryName)
    Just Category{categoryId} -> do
      addToCategory (namespace, packageName) categoryId
