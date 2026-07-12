{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Category.Update where

import Control.Monad (void)
import Database.PostgreSQL.Simple.SqlQQ
import Effectful

import Flora.Database
import Flora.Model.Category.Types
import Flora.Model.Package.Types

insertCategory :: WriteDB :> es => Category -> Eff es ()
insertCategory category = do
  void $ execute q category
  where
    q =
      [sql|
          insert into categories (category_id, name, slug, synopsis)
            values (?, ?, ?, ?)
          on conflict do nothing
        |]

-- | Adds a package to a category. Adding a package to an already-assigned category has no effect
addToCategory :: (IOE :> es, WriteDB :> es) => PackageId -> CategoryId -> Eff es ()
addToCategory packageId categoryId = (void . execute q) (packageId, categoryId)
  where
    q =
      [sql|
        insert into package_categories (package_id, category_id) values (?, ?)
        on conflict do nothing
      |]
