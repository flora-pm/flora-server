{-# LANGUAGE RecordWildCards #-}

module Flora.Model.PackageUploader.Types where

import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Text (Text)
import Data.Text.Display
import Data.UUID (UUID)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Effectful
import GHC.Generics (Generic)
import Heptapod qualified
import Web.HttpApiData

import Flora.Model.PackageIndex.Types (PackageIndex, PackageIndexId)
import Flora.Model.User (UserId)

newtype PackageUploaderId = PackageUploaderId {getPackageUploaderId :: UUID}
  deriving stock (Generic)
  deriving
    (Display)
    via ShowInstance UUID
  deriving
    (Eq, FromField, FromHttpApiData, FromJSON, NFData, Ord, Show, ToField, ToHttpApiData, ToJSON)
    via UUID

data PackageUploader = PackageUploader
  { packageUploaderId :: PackageUploaderId
  , username :: Text
  , packageIndex :: PackageIndex
  , userId :: Maybe UserId
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data PackageUploaderDAO = PackageUploaderDAO
  { packageUploaderId :: PackageUploaderId
  , username :: Text
  , packageIndexId :: PackageIndexId
  , userId :: Maybe UserId
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "package_uploaders"] PackageUploaderDAO)

mkPackageUploaderDAO
  :: IOE :> es
  => Text
  -> PackageIndexId
  -> Maybe UserId
  -> Eff es PackageUploaderDAO
mkPackageUploaderDAO username packageIndexId userId = do
  packageUploaderId <- liftIO $ PackageUploaderId <$> Heptapod.generate
  pure PackageUploaderDAO{..}
