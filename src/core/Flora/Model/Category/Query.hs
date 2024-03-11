{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Category.Query where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))

import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Flora.Model.Category.Types
import Flora.Model.Package.Types

getCategoryById :: DB :> es => CategoryId -> Eff es (Maybe Category)
getCategoryById categoryId = dbtToEff $ selectById (Only categoryId)

getCategoryBySlug :: DB :> es => Text -> Eff es (Maybe Category)
getCategoryBySlug slug = dbtToEff $ selectOneByField [field| slug |] (Only slug)

getCategoryByName :: DB :> es => Text -> Eff es (Maybe Category)
getCategoryByName categoryName = dbtToEff $ selectOneByField [field| name |] (Only categoryName)

getPackagesFromCategorySlug :: DB :> es => Text -> Eff es (Vector Package)
getPackagesFromCategorySlug slug =
  do
    getCategoryBySlug slug
    >>= \case
      Nothing -> pure Vector.empty
      Just Category{categoryId} -> do
        dbtToEff $
          joinSelectOneByField @Package @PackageCategory
            [field| package_id |]
            [field| category_id |]
            categoryId

getAllCategories :: DB :> es => Eff es (Vector Category)
getAllCategories = dbtToEff $ query_ Select (_select @Category)
