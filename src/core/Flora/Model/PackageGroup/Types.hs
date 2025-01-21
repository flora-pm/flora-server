module Flora.Model.PackageGroup.Types where

import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Text (Text)
import Data.Text.Display
import Data.UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types (GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow)
import GHC.Generics

newtype PackageGroupId = PackageGroupId {getPackageGroupId :: UUID}
  deriving
    (Display)
    via ShowInstance UUID
  deriving
    (Eq, FromField, FromJSON, NFData, Ord, Show, ToField, ToJSON)
    via UUID

data PackageGroup = PackageGroup
  { packageGroupId :: PackageGroupId
  , groupName :: Text
  }
  deriving stock
    (Eq, Generic, Ord, Show)
  deriving anyclass
    (FromJSON, FromRow, NFData, ToJSON, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "package_groups"] PackageGroup)
