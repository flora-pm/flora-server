{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Category.Update where

import Control.Monad (void)
import Data.Set qualified as Set
import Database.PostgreSQL.Entity (_insert)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.SqlQQ
import Effectful

import Flora.Database
import Flora.Model.Category.Types
import Flora.Model.Package.Types

insertCategory :: WriteDB :> es => Category -> Eff es ()
insertCategory category = do
  void $ execute insertCategoryQuery category

bulkInsertCategories :: WriteDB :> es => [Category] -> Eff es ()
bulkInsertCategories categories =
  void $ executeMany insertCategoryQuery categories

insertCategoryQuery :: Query
insertCategoryQuery =
  [sql|
  INSERT INTO categories (category_id
                        , name
                        , slug
                        , synopsis)
  VALUES (?, ?, ?, ?)
  ON CONFLICT DO NOTHING
    |]

-- | Adds a package to many categories in one statement. Adding a package to an
-- already-assigned category has no effect.
bulkAddToCategory :: WriteDB :> es => PackageId -> [CategoryId] -> Eff es ()
bulkAddToCategory packageId categoryIds =
  void $
    executeMany
      (_insert @PackageCategory <> " ON CONFLICT DO NOTHING")
      (PackageCategory packageId <$> categoryInsertOrder categoryIds)

-- | The rows of 'bulkAddToCategory' are deduplicated and ordered by 'CategoryId',
-- so that concurrent importers of one package take the row locks in one order.
categoryInsertOrder :: [CategoryId] -> [CategoryId]
categoryInsertOrder = Set.toAscList . Set.fromList
