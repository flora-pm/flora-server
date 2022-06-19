module Flora.Model.Release where

import qualified Crypto.Hash.MD5 as MD5
import Data.Aeson
import Data.Aeson.Orphans ()
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Display
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.UUID (UUID, fromByteString, toByteString)
import Database.PostgreSQL.Entity.Types (Entity, GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Distribution.SPDX.License ()
import qualified Distribution.SPDX.License as SPDX
import Distribution.Types.Version
import Distribution.Utils.Structured (structuredEncode)
import GHC.Generics (Generic)
import Optics.Core

import Flora.Model.Package
import Flora.Model.Release.Orphans ()

newtype ReleaseId = ReleaseId {getReleaseId :: UUID}
  deriving
    (Eq, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID
  deriving
    (Display)
    via ShowInstance UUID

-- | Generates a release id deterministically by hashing the package id and a version
deterministicReleaseId :: PackageId -> Version -> ReleaseId
deterministicReleaseId (PackageId packageId) version =
  ReleaseId . fromJust . fromByteString . fromStrict . MD5.hash . toStrict $ concatenatedBs
  where
    concatenatedBs = packageIdBs <> versionBs
    versionBs = structuredEncode version
    packageIdBs = toByteString packageId

data Release = Release
  { releaseId :: ReleaseId
  -- ^ The unique ID of this release
  , packageId :: PackageId
  -- ^ The package to which this release is linked
  , version :: Version
  -- ^ The version that this release represents
  , metadata :: ReleaseMetadata
  -- ^ Metadata associated with this release
  , archiveChecksum :: Text
  -- ^ The SHA256 checksum of the stored archive for this release
  , uploadedAt :: Maybe UTCTime
  , --  ^ The timestamp of upload, provided by Hackage
    createdAt :: UTCTime
  -- ^ Date of creation of this release
  , updatedAt :: UTCTime
  -- ^ Last update timestamp for this release
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "releases"] Release)

instance Ord Release where
  compare x y = compare (x ^. #version) (y ^. #version)

data ReleaseMetadata = ReleaseMetadata
  { license :: SPDX.License
  , sourceRepos :: [Text]
  , homepage :: Maybe Text
  , documentation :: Text
  , bugTracker :: Maybe Text
  , maintainer :: Text
  , synopsis :: Text
  }
  deriving stock (Eq, Ord, Show, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON)
  deriving (ToField, FromField) via Aeson ReleaseMetadata
