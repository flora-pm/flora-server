{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
module Flora.Model.Package.Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple.FromField (FromField (..), fromJSONField)
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToField (ToField (..), toJSONField)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import qualified Distribution.SPDX.License as SPDX
import GHC.Generics

import Data.Data
import Flora.Model.Package.Orphans ()
import Flora.Model.User

newtype PackageId = PackageId { getPackageId :: UUID }
  deriving stock (Generic)
  deriving (Eq, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID

newtype PackageName = PackageName Text

data Package = Package
  { packageId :: PackageId
  , namespace :: Text
  , name      :: Text
  , synopsis  :: Text
  , metadata  :: PackageMetadata
  , ownerId   :: UserId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Show,Generic)
  deriving anyclass (FromRow, ToRow)

instance Entity Package where
  tableName = "packages"
  primaryKey = [field| package_id |]
  fields = [ [field| package_id |]
           , [field| namespace |]
           , [field| name |]
           , [field| synopsis |]
           , [field| metadata :: jsonb |]
           , [field| owner_id |]
           , [field| created_at |]
           , [field| updated_at |]
           ]

data PackageMetadata = PackageMetadata
  { license       :: SPDX.License
  , sourceRepo    :: Text
  , homepage      :: Maybe Text
  , documentation :: Text
  , bugTracker    :: Maybe Text
  }
  deriving stock (Eq, Show, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON)

instance FromField PackageMetadata where
  fromField = fromJSONField

instance ToField PackageMetadata where
  toField = toJSONField

data Dependant = Dependant
  { name        :: Text
  , namespace   :: Text
  , dependantId :: PackageId
  }
  deriving stock (Eq, Show,Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[TableName "dependants"] Dependant)
