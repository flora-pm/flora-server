{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Package.Types where

import qualified Crypto.Hash.MD5 as MD5
import Data.Aeson
import Data.Aeson.Orphans ()
import Data.Data
import Data.Text (Text, unpack)
import Data.Text.Display
import Data.Time (UTCTime)
import Data.UUID
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple.FromField (FromField (..), ResultError (ConversionFailed, UnexpectedNull), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Distribution.Pretty (Pretty (..))
import qualified Distribution.SPDX.License as SPDX
import GHC.Generics
import qualified Language.Souffle.Interpreted as Souffle
import Lucid
import qualified Text.PrettyPrint as PP
import Text.Regex.Pcre2

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromJust)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector
import Flora.Model.Package.Orphans ()
import Flora.Model.User
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

newtype PackageId = PackageId {getPackageId :: UUID}
  deriving stock (Generic)
  deriving
    (Eq, Ord, Show, FromField, ToField, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData)
    via UUID
  deriving
    (Display)
    via ShowInstance UUID

-- | Generates a package id deterministically by hashing the namespace and the package name
deterministicPackageId :: Namespace -> PackageName -> PackageId
deterministicPackageId (Namespace ns) (PackageName name) =
  PackageId . fromJust . fromByteString . fromStrict . MD5.hash . encodeUtf8 $ ns <> name

newtype PackageName = PackageName Text
  deriving stock (Show, Generic)
  deriving anyclass (Souffle.Marshal)
  deriving
    (Eq, Ord, FromJSON, ToJSON, FromField, ToField, ToHtml)
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
  deriving
    (Eq, Ord, FromJSON, ToJSON, FromField, ToField, ToHtml)
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

data PackageStatus = UnknownPackage | FullyImportedPackage
  deriving stock (Eq, Show, Generic, Bounded, Enum, Ord)

parsePackageStatus :: ByteString -> Maybe PackageStatus
parsePackageStatus "unknown" = pure UnknownPackage
parsePackageStatus "fully-imported" = pure FullyImportedPackage
parsePackageStatus _ = Nothing

instance Display PackageStatus where
  displayBuilder UnknownPackage = "unknown"
  displayBuilder FullyImportedPackage = "fully-imported"

instance FromField PackageStatus where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField _ (Just bs) | Just status <- parsePackageStatus bs = pure status
  fromField f (Just bs) = returnError ConversionFailed f $ unpack $ "Conversion error: Expected component to be one of " <> display @[PackageStatus] [minBound .. maxBound] <> ", but instead got " <> decodeUtf8 bs

instance ToField PackageStatus where
  toField = Escape . encodeUtf8 . display

data Package = Package
  { packageId :: PackageId
  , namespace :: Namespace
  , name :: PackageName
  , synopsis :: Maybe Text
  , metadata :: Maybe PackageMetadata
  , ownerId :: UserId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , status :: PackageStatus
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromRow, ToRow)

instance Entity Package where
  tableName = "packages"
  primaryKey = [field| package_id |]
  fields =
    [ [field| package_id |]
    , [field| namespace |]
    , [field| name |]
    , [field| synopsis |]
    , [field| metadata :: jsonb |]
    , [field| owner_id |]
    , [field| created_at |]
    , [field| updated_at |]
    , [field| status |]
    ]

packageFieldsToUpsert :: Vector Field
packageFieldsToUpsert =
  [ [field| synopsis |]
  , [field| metadata :: jsonb |]
  , [field| updated_at |]
  , [field| status |]
  ]

data PackageMetadata = PackageMetadata
  { license :: SPDX.License
  , sourceRepos :: [Text]
  , homepage :: Maybe Text
  , documentation :: Text
  , bugTracker :: Maybe Text
  , maintainer :: Text
  }
  deriving stock (Eq, Ord, Show, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON)
  deriving (ToField, FromField) via Aeson PackageMetadata

data Dependent = Dependent
  { name :: Text
  , namespace :: Text
  , dependentId :: PackageId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "dependents"] Dependent)
