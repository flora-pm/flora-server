{-# LANGUAGE TemplateHaskell #-}

module Flora.Model.Release.Types
  ( ReleaseId (..)
  , TextHtml
  , Release (..)
  , ImportStatus (..)
  , SupportedCompilers (..)
  , ReleaseDeprecation (..)
  , ReleaseFlags (..)
  )
where

import Control.DeepSeq
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.OpenApi.Schema (ToSchema)
import Data.Text (Text, unpack)
import Data.Text.Display
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.Types (Entity, GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (..), ResultError (..), returnError)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))
import Deriving.Aeson
import Distribution.Compiler (CompilerFlavor)
import Distribution.SPDX.License ()
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.BuildType (BuildType)
import Distribution.Types.Flag (PackageFlag)
import Distribution.Types.Version
import Distribution.Types.VersionRange (VersionRange)

import Data.Aeson.Orphans ()
import Data.Text.HTML
import Distribution.Orphans ()
import Distribution.Orphans.BuildType ()
import Distribution.Orphans.CompilerFlavor ()
import Distribution.Orphans.PackageFlag ()
import Flora.Model.BlobStore.Types
import Flora.Model.Package

newtype ReleaseId = ReleaseId {getReleaseId :: UUID}
  deriving
    (Display)
    via ShowInstance UUID
  deriving
    (Eq, FromField, FromJSON, NFData, Ord, Show, ToField, ToJSON)
    via UUID

data Release = Release
  { releaseId :: ReleaseId
  , packageId :: PackageId
  , version :: Version
  , archiveChecksum :: Maybe Text
  , uploadedAt :: Maybe UTCTime
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , readme :: Maybe TextHtml
  , readmeStatus :: ImportStatus
  , changelog :: Maybe TextHtml
  , changelogStatus :: ImportStatus
  , tarballRootHash :: Maybe Sha256Sum
  , tarballArchiveHash :: Maybe Sha256Sum
  , license :: SPDX.License
  , sourceRepos :: Vector Text
  , homepage :: Maybe Text
  , documentation :: Text
  , bugTracker :: Maybe Text
  , maintainer :: Text
  , synopsis :: Text
  , description :: Text
  , flags :: ReleaseFlags
  , testedWith :: Vector Version
  , deprecated :: Maybe Bool
  , repository :: Maybe Text
  , revisedAt :: Maybe UTCTime
  , buildType :: BuildType
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "releases"] Release)

instance Ord Release where
  compare x y = compare x.version y.version

newtype ReleaseFlags = ReleaseFlags (Vector PackageFlag)
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, NFData, ToJSON, ToSchema)
  deriving (FromField, ToField) via Aeson ReleaseFlags

data ImportStatus
  = Imported
  | Inexistent
  | NotImported
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (NFData)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] ImportStatus)

parseImportStatus :: ByteString -> Maybe ImportStatus
parseImportStatus "imported" = pure Imported
parseImportStatus "inexistent" = pure Inexistent
parseImportStatus "not-imported" = pure NotImported
parseImportStatus _ = Nothing

instance Display ImportStatus where
  displayBuilder Imported = "imported"
  displayBuilder Inexistent = "inexistent"
  displayBuilder NotImported = "not-imported"

instance FromField ImportStatus where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField _ (Just bs) | Just status <- parseImportStatus bs = pure status
  fromField f (Just bs) =
    returnError ConversionFailed f $
      unpack $
        "Conversion error: Expected component to be one of "
          <> display @[ImportStatus] [minBound .. maxBound]
          <> ", but instead got "
          <> decodeUtf8 bs

instance ToField ImportStatus where
  toField = Escape . encodeUtf8 . display

newtype SupportedCompilers = Vector (CompilerFlavor, VersionRange)
  deriving stock (Eq, Generic, Show, Typeable)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] SupportedCompilers)

data ReleaseDeprecation = ReleaseDeprecation
  { deprecated :: Bool
  , release :: ReleaseId
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] ReleaseDeprecation)
  deriving (FromField, ToField) via Aeson ReleaseDeprecation

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Release)
