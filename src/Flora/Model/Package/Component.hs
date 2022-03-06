{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
module Flora.Model.Package.Component
  ( ComponentId(..)
  , PackageComponent(..)
  , ComponentType(..)
  , CanonicalComponent(..)
  ) where

import Data.Aeson
import Data.Aeson.Orphans ()
import Data.ByteString
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display
import Data.Text.Encoding
import qualified Data.Text.Lazy.Builder as B
import Data.UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField (..), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import GHC.Generics
import Optics.Core

import Flora.Model.Release

newtype ComponentId = ComponentId { getComponentId :: UUID }
  deriving stock (Generic)
  deriving (Eq, Ord, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID
  deriving Display via ShowInstance UUID

data ComponentType
  = Library
  | Executable
  | TestSuite
  | Benchmark
  | ForeignLib
  deriving stock (Eq, Show, Generic, Bounded, Enum)

instance Display ComponentType where
  displayBuilder Library    = "library"
  displayBuilder Executable = "executable"
  displayBuilder TestSuite  = "test"
  displayBuilder Benchmark  = "benchmark"
  displayBuilder ForeignLib = "foreign-library"

instance FromField ComponentType where
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs ->
        case parseComponentType bs of
          Just a -> pure a
          Nothing -> returnError ConversionFailed f $ T.unpack $  "Conversion error: Expected component to be one of " <> display @[ComponentType] [minBound .. maxBound] <> ", but instead got " <> decodeUtf8 bs

parseComponentType :: ByteString -> Maybe ComponentType
parseComponentType "library"         = Just Library
parseComponentType "executable"      = Just Executable
parseComponentType "test"            = Just TestSuite
parseComponentType "benchmark"       = Just Benchmark
parseComponentType "foreign-library" = Just ForeignLib
parseComponentType _                 = Nothing

instance ToField ComponentType where
  toField = Escape . encodeUtf8 . display

data CanonicalComponent = CanonicalComponent
  { componentName :: Text
  , componentType :: ComponentType
  }
  deriving stock (Eq, Show, Generic)

instance Display CanonicalComponent where
  displayBuilder CanonicalComponent{componentName, componentType} = displayBuilder componentType <> ":" <> B.fromText componentName

data PackageComponent = PackageComponent
  { componentId   :: ComponentId
  , releaseId     :: ReleaseId
  , canonicalForm :: CanonicalComponent
  }
  deriving stock (Eq, Show, Generic)
  deriving Display via ShowInstance PackageComponent

instance Entity PackageComponent where
  tableName = "package_components"
  primaryKey = [field| package_component_id |]
  fields = [ [field| package_component_id |]
           , [field| release_id |]
           , [field| component_name |]
           , [field| component_type |]
           ]

instance ToRow PackageComponent where
  toRow PackageComponent{componentId, releaseId, canonicalForm} =
    let componentId' = componentId
        releaseId' = releaseId
        componentName' = canonicalForm ^. #componentName
        componentType' = canonicalForm ^. #componentType
     in toRow PackageComponent'{..}

instance FromRow PackageComponent where
  fromRow = do
    PackageComponent'{..} <- fromRow
    let canonicalForm = CanonicalComponent componentName' componentType'
    pure $ PackageComponent componentId' releaseId' canonicalForm

-- | Data Access Object used to serialise to the DB
data PackageComponent' = PackageComponent'
  { componentId'   :: ComponentId
  , releaseId'     :: ReleaseId
  , componentName' :: Text
  , componentType' :: ComponentType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToRow, FromRow)
