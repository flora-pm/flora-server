module Flora.Model.PackageGroupPackage.Types where

import GHC.Generics

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
import Flora.Model.Package.Types (PackageId)
import Flora.Model.PackageGroup.Types (PackageGroupId)

newtype PackageGroupPackageId = PackageGroupPackageId {getPackageGroupPackageId :: UUID}
  deriving
    (Eq, Ord, Show, FromField, ToField, FromJSON, ToJSON, NFData)
    via UUID
  deriving
    (Display)
    via ShowInstance UUID

data PackageGroupPackage = PackageGroupPackage
  { packageGroupPackageId :: PackageGroupPackageId
  , packageId :: PackageId
  , packageGroupId :: PackageGroupId
  }
  deriving stock
    (Eq, Ord, Show, Generic)
  deriving anyclass
    (FromRow, ToRow, FromJSON, ToJSON, NFData)
  deriving
    (Entity)
    via (GenericEntity '[TableName "package_group_packages"] PackageGroupPackage)
