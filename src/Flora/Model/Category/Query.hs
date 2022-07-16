{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Category.Query where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))

import qualified Data.Text.IO as T
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Flora.Model.Category.Types
import Flora.Model.Package.Types

getCategoryById :: ([DB, IOE] :>> es) => CategoryId -> Eff es (Maybe Category)
getCategoryById categoryId = dbtToEff $ selectById (Only categoryId)

getCategoryBySlug :: ([DB, IOE] :>> es) => Text -> Eff es (Maybe Category)
getCategoryBySlug slug = dbtToEff $ selectOneByField [field| slug |] (Only slug)

getCategoryByName :: ([DB, IOE] :>> es) => Text -> Eff es (Maybe Category)
getCategoryByName categoryName = dbtToEff $ selectOneByField [field| name |] (Only categoryName)

getPackagesFromCategorySlug :: ([DB, IOE] :>> es) => Text -> Eff es (Vector Package)
getPackagesFromCategorySlug slug =
  do
    getCategoryBySlug slug
    >>= \case
      Nothing -> do
        liftIO $ T.putStrLn $ "Could not find category from slug: \"" <> slug <> "\""
        pure Vector.empty
      Just Category{categoryId} -> do
        liftIO $ T.putStrLn "Category found!"
        dbtToEff $
          query
            Select
            [sql|
        select  p.package_id
              , p.namespace
              , p.name
              , p.owner_id
              , p.created_at
              , p.updated_at
              , p.status
        from packages as p
        inner join package_categories as pc on (p.package_id = pc.package_id)
        where pc.category_id = ?
        |]
            (Only categoryId)

getAllCategories :: ([DB, IOE] :>> es) => Eff es (Vector Category)
getAllCategories = dbtToEff $ query_ Select (_select @Category)
