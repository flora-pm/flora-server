module Flora.Downloads where

import Data.UUID
import GHC.Generics
import Data.Aeson
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Flora.Release
import Data.Time (UTCTime)

newtype DownloadId = DownloadId { getDownloadId :: UUID }
  deriving stock (Generic, Show)
  deriving (Eq, Ord, FromJSON, ToJSON, FromField, ToField)
    via UUID

data Download = Download
  { downloadId :: DownloadId
  , releaseId :: ReleaseId
  , downloads :: Integer
  , day :: UTCTime
  }
