module Flora.Model.Release where

import qualified Crypto.Hash.MD5 as MD5
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString
import Data.Time (UTCTime)
import Data.UUID (UUID, fromByteString, toByteString)
import Database.PostgreSQL.Entity.Types (Entity, GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Distribution.Types.Version
import GHC.Generics (Generic)

import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Maybe (fromJust)
import Data.Text.Display
import Distribution.Utils.Structured (structuredEncode)
import Flora.Model.Package
import Flora.Model.Release.Orphans ()
import Optics.Core

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
  , archiveChecksum :: ByteString
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
    Entity
    via (GenericEntity '[TableName "releases"] Release)

instance Ord Release where
  compare x y = compare (x ^. #version) (y ^. #version)
