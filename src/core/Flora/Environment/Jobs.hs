module Flora.Environment.Jobs where

import Data.ByteString
import Data.Pool
import Database.PostgreSQL.Simple qualified as PG
import GHC.Generics (Generic)

data FloraJobsEnv = FloraJobsEnv
  { pool :: Pool PG.Connection
  , connectionString :: StrictByteString
  }
  deriving stock (Generic)
