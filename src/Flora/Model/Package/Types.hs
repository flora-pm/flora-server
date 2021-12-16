{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}

module Flora.Model.Package.Types where

import Data.Aeson
import Data.Aeson.Orphans ()
import Data.Text (Text, unpack)
import Data.Text.Display
import Data.Time (UTCTime)
import Data.UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Distribution.Pretty (Pretty (..))
import qualified Distribution.SPDX.License as SPDX
import GHC.Generics
import Lucid
import qualified Text.PrettyPrint as PP
import Text.Regex.Pcre2

import Data.Data
import Database.PostgreSQL.Simple.Newtypes
import Flora.Model.Package.Orphans ()
import Flora.Model.User

newtype PackageId = PackageId { getPackageId :: UUID }
  deriving stock (Generic)
  deriving (Eq, Ord, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID

newtype PackageName = PackageName Text
  deriving stock (Show)
  deriving (Eq, Ord, FromJSON, ToJSON, FromField, ToField, ToHtml)
    via Text

instance Pretty PackageName where
  pretty (PackageName txt) = PP.text $ unpack txt

instance Display PackageName where
  displayBuilder (PackageName name) = displayBuilder name

parsePackageName :: Text -> Maybe PackageName
parsePackageName txt =
  if matches "[[:digit:]]*[[:alpha:]][[:alnum:]]*(-[[:digit:]]*[[:alpha:]][[:alnum:]]*)*" txt
  then Just $ PackageName txt
  else Nothing

newtype Namespace = Namespace Text
  deriving stock (Show)
  deriving (Eq, Ord, FromJSON, ToJSON, FromField, ToField, ToHtml)
    via Text

instance Pretty Namespace where
  pretty (Namespace txt) = PP.text $ unpack txt

instance Display Namespace where
  displayBuilder (Namespace name) = displayBuilder name

parseNamespace :: Text -> Maybe Namespace
parseNamespace txt =
  if matches "[[:digit:]]*[[:alpha:]][[:alnum:]]*(-[[:digit:]]*[[:alpha:]][[:alnum:]]*)*" txt
  then Just $ Namespace txt
  else Nothing

data Package = Package
  { packageId :: PackageId
  , namespace :: Namespace
  , name      :: PackageName
  , synopsis  :: Text
  , metadata  :: PackageMetadata
  , ownerId   :: UserId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Ord, Show ,Generic)
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
  deriving stock (Eq, Ord, Show, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON)
   deriving (ToField, FromField) via Aeson PackageMetadata


data Dependent = Dependent
  { name        :: Text
  , namespace   :: Text
  , dependentId :: PackageId
  }
  deriving stock (Eq, Show,Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[TableName "dependents"] Dependent)
