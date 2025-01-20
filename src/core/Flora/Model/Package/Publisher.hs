module Flora.Model.Package.Publisher where

import Data.UUID (UUID)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact
import GHC.Generics

import Flora.Model.Package
import Flora.Model.User (UserId)

newtype PackagePublisherId = PackagePublisherId {getPackagePublisherId :: UUID}
  deriving
    (Eq, FromField, Show, ToField)
    via UUID

data PackagePublisher = PackagePublisher
  { packagePublisherId :: PackagePublisherId
  , packageNamespace :: Namespace
  , packageName :: PackageName
  , publisherId :: UserId
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "package_publishers"] PackagePublisher)

insertPackagePublisher :: PackagePublisher -> DBT IO ()
insertPackagePublisher p = insert @PackagePublisher p
