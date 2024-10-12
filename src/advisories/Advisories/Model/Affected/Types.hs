{-# LANGUAGE QuasiQuotes #-}

module Advisories.Model.Affected.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics

import Advisories.Model.Advisory.Types
import Flora.Model.Package.Types
import Flora.Model.Release.Types

newtype AffectedPackageId = AffectedPackageId {getAffectedPackageId :: UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, FromJSON, ToJSON, FromField, ToField, NFData)
    via UUID

data AffectedPackageDAO = AffectedPackageDAO
  { affectedPackageId :: AffectedPackageId
  , advisoryId :: AdvisoryId
  , packageId :: PackageId
  , cvss :: Text
  , introducedVersion :: ReleaseId
  , fixedVersion :: Maybe ReleaseId
  , architectures :: Vector Text
  , operatingSystems :: Vector Text
  , declarations :: Vector (Vector (Text))
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[TableName "affected_packages"] AffectedPackageDAO)
