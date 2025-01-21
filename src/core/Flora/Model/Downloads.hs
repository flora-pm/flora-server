module Flora.Model.Downloads where

import Data.Aeson
import Data.Time (UTCTime)
import Data.UUID
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics

import Flora.Model.Release.Types

newtype DownloadId = DownloadId {getDownloadId :: UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, FromField, FromJSON, Ord, ToField, ToJSON)
    via UUID

data Download = Download
  { downloadId :: DownloadId
  , releaseId :: ReleaseId
  , downloads :: Integer
  , day :: UTCTime
  }
