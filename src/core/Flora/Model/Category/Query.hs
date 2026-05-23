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
import Effectful.Labeled
import Effectful.PostgreSQL

import Flora.Database
import Flora.Model.Category.Types
import Flora.Model.Package.Types

getCategoryById :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => CategoryId -> Eff es (Maybe Category)
getCategoryById categoryId = labeled @ReadOnly @WithConnection $ queryOne (_selectWhere @Category [primaryKey @Category]) (Only categoryId)

getCategoryBySlug :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => Text -> Eff es (Maybe Category)
getCategoryBySlug slug = labeled @ReadOnly @WithConnection $ queryOne (_selectWhere @Category [[field| slug |]]) (Only slug)

getCategoryByName :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => Text -> Eff es (Maybe Category)
getCategoryByName categoryName = labeled @ReadOnly @WithConnection $ queryOne (_selectWhere @Category [[field| name |]]) (Only categoryName)

getPackagesFromCategorySlug :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => Text -> Eff es (Vector Package)
getPackagesFromCategorySlug slug =
  do
    getCategoryBySlug slug
    >>= \case
      Nothing -> do
        liftIO $ T.putStrLn $ "Could not find category from slug: \"" <> slug <> "\""
        pure Vector.empty
      Just Category{categoryId} -> do
        labeled @ReadOnly @WithConnection $
          Vector.fromList
            <$> query (_joinSelectOneByField @Package @PackageCategory [field| package_id |] [field| category_id |]) (Only categoryId)

getAllCategories :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => Eff es (Vector Category)
getAllCategories = labeled @ReadOnly @WithConnection $ Vector.fromList <$> query_ (_select @Category)
