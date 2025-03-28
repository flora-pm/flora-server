module Flora.Model.PackageGroupPackage.Types where

import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Text.Display
import Data.UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types (GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow)
import GHC.Generics

import Flora.Model.Package.Types (PackageId)
import Flora.Model.PackageGroup.Types (PackageGroupId)

newtype PackageGroupPackageId = PackageGroupPackageId {getPackageGroupPackageId :: UUID}
  deriving
    (Display)
    via ShowInstance UUID
  deriving
    (Eq, FromField, FromJSON, NFData, Ord, Show, ToField, ToJSON)
    via UUID

data PackageGroupPackage = PackageGroupPackage
  { packageGroupPackageId :: PackageGroupPackageId
  , packageId :: PackageId
  , packageGroupId :: PackageGroupId
  }
  deriving stock
    (Eq, Generic, Ord, Show)
  deriving anyclass
    (FromJSON, FromRow, NFData, ToJSON, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "package_group_packages"] PackageGroupPackage)
