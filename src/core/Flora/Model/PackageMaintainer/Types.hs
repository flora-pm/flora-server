module Flora.Model.PackageMaintainer.Types where

import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Text.Display
import Data.Time
import Data.UUID (UUID)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Effectful
import Effectful.Time
import Effectful.Time qualified as Time
import GHC.Generics (Generic)
import Heptapod qualified
import Web.HttpApiData

import Flora.Model.Package.Types
import Flora.Model.PackageUploader.Types

newtype PackageMaintainerId = PackageMaintainerId {getPackageMaintainerId :: UUID}
  deriving stock (Generic)
  deriving
    (Display)
    via ShowInstance UUID
  deriving
    (Eq, FromField, FromHttpApiData, FromJSON, NFData, Ord, Show, ToField, ToHttpApiData, ToJSON)
    via UUID

data PackageMaintainer = PackageMaintainer
  { packageMaintainerId :: PackageMaintainerId
  , packageUploaderId :: PackageUploaderId
  , packageId :: PackageId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "package_maintainers"] PackageMaintainer)

mkPackageMaintainer
  :: (IOE :> es, Time :> es)
  => PackageUploaderId
  -> PackageId
  -> Eff es PackageMaintainer
mkPackageMaintainer packageUploaderId packageId = do
  packageMaintainerId <- liftIO $ PackageMaintainerId <$> Heptapod.generate
  createdAt <- Time.currentTime
  pure
    PackageMaintainer
      { packageMaintainerId
      , packageUploaderId
      , packageId
      , createdAt
      , updatedAt = createdAt
      }
