module Flora.Model.Downloads where

import Data.Aeson
import Data.Time (UTCTime)
import Data.UUID
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Flora.Model.Release
import GHC.Generics

newtype DownloadId = DownloadId {getDownloadId :: UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, FromJSON, ToJSON, FromField, ToField)
    via UUID

data Download = Download
  { downloadId :: DownloadId
  , releaseId :: ReleaseId
  , downloads :: Integer
  , day :: UTCTime
  }
