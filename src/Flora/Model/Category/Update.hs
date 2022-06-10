module Flora.Model.Category.Update where

import Control.Monad.IO.Class
import Data.Text (Text)
import Database.PostgreSQL.Entity (insert)
import Database.PostgreSQL.Transact (DBT)

import Control.Monad (void)
import qualified Data.Text.IO as T
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute)
import Database.PostgreSQL.Simple.SqlQQ
import qualified Flora.Model.Category.Query as Query
import Flora.Model.Category.Types
import Flora.Model.Package.Types

insertCategory :: (MonadIO m) => Category -> DBT m ()
insertCategory category = insert @Category category

-- | Adds a package to a category. Adding a package to an already-assigned category has no effect
addToCategory :: (MonadIO m) => PackageId -> CategoryId -> DBT m ()
addToCategory packageId categoryId = void . execute Update q $ (packageId, categoryId)
  where
    q =
      [sql| 
        insert into package_categories (package_id, category_id) values (?, ?)  
        on conflict do nothing
      |]

addToCategoryByName :: (MonadIO m) => PackageId -> Text -> DBT m ()
addToCategoryByName packageId categoryName = do
  mCategory <- Query.getCategoryByName categoryName
  case mCategory of
    Nothing -> do
      liftIO $ T.putStrLn ("Could not find category " <> categoryName)
    Just Category{categoryId} -> do
      addToCategory packageId categoryId
