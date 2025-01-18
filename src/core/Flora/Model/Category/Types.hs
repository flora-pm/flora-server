{-# LANGUAGE RoleAnnotations #-}

module Flora.Model.Category.Types where

import Control.DeepSeq
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Flora.Model.Package.Types
import GHC.Generics
import Servant
import Text.Slugify

newtype CategoryId = CategoryId {getCategoryId :: UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, FromJSON, ToJSON, FromField, ToField, FromHttpApiData, ToHttpApiData, NFData)
    via UUID

newtype CategoryName = CategoryName {getCategoryName :: Text}
  deriving stock (Show, Generic)
  deriving
    (Eq, Ord, NFData)
    via Text

data Category = Category
  { categoryId :: CategoryId
  , name :: Text
  , slug :: Text
  , synopsis :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[TableName "categories"] Category)

data PackageCategory = PackageCategory
  { packageId :: PackageId
  , categoryId :: CategoryId
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[TableName "package_categories"] PackageCategory)

mkCategoryId :: IO CategoryId
mkCategoryId = CategoryId <$> UUID.nextRandom

mkCategory
  :: CategoryId
  -- ^ Id of the category in the database
  -> Text
  -- ^ Name
  -> Maybe Text
  -- ^ Optional slug, can be inferred from the name
  -> Text
  -- ^ Synopsis
  -> Category
mkCategory categoryId name Nothing synopsis =
  mkCategory categoryId name (Just $ slugify name) synopsis
mkCategory categoryId name (Just slug) synopsis =
  Category{..}
