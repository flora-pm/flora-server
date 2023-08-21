{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Flora.Model.Component.Types
  ( ComponentId (..)
  , PackageComponent (..)
  , ComponentType (..)
  , CanonicalComponent (..)
  , ComponentCondition (..)
  , ComponentMetadata (..)
  , deterministicComponentId
  , PackageWithLatestRelease (..)
  )
where

import Control.DeepSeq
import Crypto.Hash.MD5 qualified as MD5
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString
import Data.Data
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display
import Data.Text.Encoding
import Data.Text.Lazy.Builder qualified as B
import Data.Time
import Data.UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField (..), fromJSONField, returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField (..), toJSONField)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Distribution.PackageDescription qualified as Condition
import Distribution.SPDX qualified as SPDX
import Distribution.Types.Version

import Data.Aeson.Orphans ()
import Distribution.Orphans ()
import Distribution.Orphans.ConfVar ()
import Flora.Model.Package.Types
import Flora.Model.Release.Types
import GHC.Generics

newtype ComponentId = ComponentId {getComponentId :: UUID}
  deriving stock (Generic)
  deriving
    (Eq, Ord, Show, FromField, ToField, FromJSON, ToJSON, NFData)
    via UUID
  deriving (Display) via ShowInstance UUID

data ComponentType
  = Library
  | Executable
  | TestSuite
  | Benchmark
  | ForeignLib
  deriving stock (Eq, Ord, Show, Generic, Bounded, Enum)
  deriving anyclass (NFData)

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''ComponentType)

instance Display ComponentType where
  displayBuilder Library = "library"
  displayBuilder Executable = "executable"
  displayBuilder TestSuite = "test"
  displayBuilder Benchmark = "benchmark"
  displayBuilder ForeignLib = "foreign-library"

instance FromField ComponentType where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField _ (Just bs) | Just status <- parseComponentType bs = pure status
  fromField f (Just bs) = returnError ConversionFailed f $ T.unpack $ "Conversion error: Expected component to be one of " <> display @[ComponentType] [minBound .. maxBound] <> ", but instead got " <> decodeUtf8 bs

parseComponentType :: ByteString -> Maybe ComponentType
parseComponentType "library" = Just Library
parseComponentType "executable" = Just Executable
parseComponentType "test" = Just TestSuite
parseComponentType "benchmark" = Just Benchmark
parseComponentType "foreign-library" = Just ForeignLib
parseComponentType _ = Nothing

instance ToField ComponentType where
  toField = Escape . encodeUtf8 . display

data CanonicalComponent = CanonicalComponent
  { componentName :: Text
  , componentType :: ComponentType
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''CanonicalComponent)

deterministicComponentId :: ReleaseId -> CanonicalComponent -> ComponentId
deterministicComponentId releaseId canonicalForm =
  ComponentId . fromJust . fromByteString . fromStrict . MD5.hash . encodeUtf8 $ concatenated
  where
    concatenated = display releaseId <> display canonicalForm

instance Display CanonicalComponent where
  displayBuilder CanonicalComponent{componentName, componentType} =
    displayBuilder componentType <> ":" <> B.fromText componentName

newtype ComponentCondition = ComponentCondition (Condition.Condition Condition.ConfVar)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

data ComponentMetadata = ComponentMetadata
  { conditions :: [ComponentCondition]
  }
  deriving stock (Eq, Show, Generic, Typeable)
  deriving anyclass (FromJSON, ToJSON, NFData)

instance FromField ComponentMetadata where
  fromField = fromJSONField

instance ToField ComponentMetadata where
  toField = toJSONField

data PackageComponent = PackageComponent
  { componentId :: ComponentId
  , releaseId :: ReleaseId
  , canonicalForm :: CanonicalComponent
  , metadata :: ComponentMetadata
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)
  deriving (Display) via ShowInstance PackageComponent

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''PackageComponent)

instance Entity PackageComponent where
  tableName = "package_components"
  primaryKey = [field| package_component_id |]
  fields =
    [ [field| package_component_id |]
    , [field| release_id |]
    , [field| component_name |]
    , [field| component_type |]
    , [field| component_metadata |]
    ]

instance ToRow PackageComponent where
  toRow PackageComponent{componentId, releaseId, canonicalForm, metadata} =
    let componentId' = componentId
        releaseId' = releaseId
        componentMetadata' = metadata
        componentName' = canonicalForm.componentName
        componentType' = canonicalForm.componentType
     in toRow PackageComponent'{..}

instance FromRow PackageComponent where
  fromRow = do
    PackageComponent'{..} <- fromRow
    let canonicalForm = CanonicalComponent componentName' componentType'
    pure $ PackageComponent componentId' releaseId' canonicalForm componentMetadata'

-- | Data Access Object used to serialise to the DB
data PackageComponent' = PackageComponent'
  { componentId' :: ComponentId
  , releaseId' :: ReleaseId
  , componentName' :: Text
  , componentType' :: ComponentType
  , componentMetadata' :: ComponentMetadata
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToRow, FromRow)

data PackageWithLatestRelease = PackageWithLatestRelease
  { namespace :: Namespace
  , packageName :: PackageName
  , version :: Version
  , license :: SPDX.License
  , sourceRepos :: Vector Text
  , homepage :: Maybe Text
  , documentation :: Text
  , bugTracker :: Maybe Text
  , synopsis :: Text
  , description :: Text
  , releaseDeprecated :: Bool
  , repository :: Maybe Text
  , uploadedAt :: UTCTime
  , flags :: ReleaseFlags
  , testedWith :: Vector Version
  , componentName :: Text
  , componentType :: ComponentType
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (FromRow)
