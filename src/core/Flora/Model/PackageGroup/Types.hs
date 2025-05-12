{-# LANGUAGE RecordWildCards #-}

module Flora.Model.PackageGroup.Types where

import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Text (Text)
import Data.Text.Display
import Data.UUID (UUID)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types (GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Effectful
import GHC.Generics
import Heptapod qualified
import Servant (FromHttpApiData, ToHttpApiData)

newtype PackageGroupId = PackageGroupId {getPackageGroupId :: UUID}
  deriving
    (Display)
    via ShowInstance UUID
  deriving
    (Eq, FromField, FromHttpApiData, FromJSON, NFData, Ord, Show, ToField, ToHttpApiData, ToJSON)
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

mkPackageGroup :: IOE :> es => Text -> Eff es PackageGroup
mkPackageGroup groupName = do
  packageGroupId <- liftIO $ PackageGroupId <$> Heptapod.generate
  pure PackageGroup{..}
