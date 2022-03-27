module Flora.Package.Publishers where

import Data.UUID (UUID)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact
import Flora.User (UserId)
import GHC.Generics

newtype PackagePublisherId = PackagePublisherId { getPackagePublisherId :: UUID }
  deriving (Eq, Show, FromField, ToField)
    via UUID

data PackagePublisher = PackagePublisher
  { packagePublisherId :: PackagePublisherId
  , packageNamespace   :: Namespace
  , packageName        :: PackageName
  , publisherId        :: UserId
  }
  deriving stock (Eq, Show,Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[TableName "package_publishers"] PackagePublisher)

insertPackagePublisher :: PackagePublisher -> DBT IO ()
insertPackagePublisher p = insert @PackagePublisher p
