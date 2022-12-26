{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Category.Update where

import Control.Monad (void)
import Data.Text (Text)
import Database.PostgreSQL.Entity (insert)
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), executeMany)
import Database.PostgreSQL.Simple.SqlQQ
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Flora.Model.Category.Query qualified as Query
import Flora.Model.Category.Types (Category (..), CategoryId)
import Flora.Model.Package.Types

insertCategory :: (DB :> es) => Category -> Eff es ()
insertCategory category = dbtToEff $ insert @Category category

-- | Adds a package to a category. Adding a package to an already-assigned category has no effect
addManyToCategory :: (DB :> es) => Vector (PackageId, CategoryId) -> Eff es ()
addManyToCategory parameters = dbtToEff $ void $ executeMany Update q (Vector.toList parameters)
  where
    q =
      [sql| 
        insert into package_categories (package_id, category_id) values (?, ?)  
        on conflict do nothing
      |]

addToCategoryByName :: (DB :> es, IOE :> es) => PackageId -> Vector Text -> Eff es ()
addToCategoryByName packageId categoryNames = do
  liftIO $ print categoryNames
  liftIO $ putStrLn "================================================================"
  categoryIds <- Query.getCategoriesByNames categoryNames >>= (\cats -> pure $ fmap (.categoryId) cats)
  if Vector.null categoryIds
    then pure ()
    else addManyToCategory $ (,) <$> Vector.singleton packageId <*> categoryIds
