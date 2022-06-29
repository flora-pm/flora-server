module Flora.Model.Release where

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Maybe (fromJust)
import Data.UUID (fromByteString, toByteString)
import Distribution.Types.Version
import Distribution.Utils.Structured (structuredEncode)

import Flora.Model.Package
import Flora.Model.Release.Types

-- | Generates a release id deterministically by hashing the package id and a version
deterministicReleaseId :: PackageId -> Version -> ReleaseId
deterministicReleaseId (PackageId packageId) version =
  ReleaseId . fromJust . fromByteString . fromStrict . MD5.hash . toStrict $ concatenatedBs
  where
    concatenatedBs = packageIdBs <> versionBs
    versionBs = structuredEncode version
    packageIdBs = toByteString packageId
