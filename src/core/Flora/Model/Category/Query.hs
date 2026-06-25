{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Category.Query where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))
import Effectful

import Flora.Database
import Flora.Model.Category.Types
import Flora.Model.Package.Types

getCategoryById :: (IOE :> es, ReadDB :> es) => CategoryId -> Eff es (Maybe Category)
getCategoryById categoryId = queryOne (_selectWhere @Category [primaryKey @Category]) (Only categoryId)

getCategoryBySlug :: (IOE :> es, ReadDB :> es) => Text -> Eff es (Maybe Category)
getCategoryBySlug slug = queryOne (_selectWhere @Category [[field| slug |]]) (Only slug)

getCategoryByName :: (IOE :> es, ReadDB :> es) => Text -> Eff es (Maybe Category)
getCategoryByName categoryName = queryOne (_selectWhere @Category [[field| name |]]) (Only categoryName)

getPackagesFromCategorySlug :: (IOE :> es, ReadDB :> es) => Text -> Eff es (Vector Package)
getPackagesFromCategorySlug slug =
  do
    getCategoryBySlug slug
    >>= \case
      Nothing -> do
        liftIO $ T.putStrLn $ "Could not find category from slug: \"" <> slug <> "\""
        pure Vector.empty
      Just Category{categoryId} -> do
        Vector.fromList
          <$> query (_joinSelectOneByField @Package @PackageCategory [field| package_id |] [field| category_id |]) (Only categoryId)

getAllCategories :: ReadDB :> es => Eff es (Vector Category)
getAllCategories = Vector.fromList <$> query_ (_select @Category)
