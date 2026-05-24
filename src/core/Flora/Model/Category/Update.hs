{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Category.Update where

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text.IO qualified as T
import Database.PostgreSQL.Simple.SqlQQ
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader

import Flora.Database
import Flora.Environment.Env
import Flora.Model.Category.Query qualified as Query
import Flora.Model.Category.Types
import Flora.Model.Package.Types
import Flora.Monad

insertCategory :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => Category -> Eff es ()
insertCategory category = do
  labeled @_ @WithConnection $ void $ execute q category
  where
    q =
      [sql|
          insert into categories (category_id, name, slug, synopsis)
            values (?, ?, ?, ?)
          on conflict do nothing
        |]

-- | Adds a package to a category. Adding a package to an already-assigned category has no effect
addToCategory :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => PackageId -> CategoryId -> Eff es ()
addToCategory packageId categoryId = labeled @_ @WithConnection $ (void . execute q) (packageId, categoryId)
  where
    q =
      [sql|
        insert into package_categories (package_id, category_id) values (?, ?)
        on conflict do nothing
      |]

addToCategoryByName :: (IOE :> es, Labeled ReadWrite WithConnection :> es, Reader FloraEnv :> es) => PackageId -> Text -> FloraM es ()
addToCategoryByName packageId categoryName = do
  FloraEnv{pool} <- Reader.ask
  mCategory <- withReadOnlyPool pool $ Query.getCategoryByName categoryName
  case mCategory of
    Nothing -> do
      liftIO $ T.putStrLn ("Could not find category " <> categoryName)
    Just Category{categoryId} -> do
      addToCategory packageId categoryId
