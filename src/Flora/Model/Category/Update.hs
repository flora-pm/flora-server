{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Category.Update where

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text.IO qualified as T
import Database.PostgreSQL.Entity (insert)
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute)
import Database.PostgreSQL.Simple.SqlQQ
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Flora.Model.Category.Query qualified as Query
import Flora.Model.Category.Types
import Flora.Model.Package.Types

insertCategory :: ([DB, IOE] :>> es) => Category -> Eff es ()
insertCategory category = dbtToEff $ insert @Category category

-- | Adds a package to a category. Adding a package to an already-assigned category has no effect
addToCategory :: ([DB, IOE] :>> es) => PackageId -> CategoryId -> Eff es ()
addToCategory packageId categoryId = dbtToEff $ void . execute Update q $ (packageId, categoryId)
  where
    q =
      [sql| 
        insert into package_categories (package_id, category_id) values (?, ?)  
        on conflict do nothing
      |]

addToCategoryByName :: ([DB, IOE] :>> es) => PackageId -> Text -> Eff es ()
addToCategoryByName packageId categoryName = do
  mCategory <- Query.getCategoryByName categoryName
  case mCategory of
    Nothing -> do
      liftIO $ T.putStrLn ("Could not find category " <> categoryName)
    Just Category{categoryId} -> do
      addToCategory packageId categoryId
