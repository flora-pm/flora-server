module Flora.Environment.Jobs where

import Data.ByteString
import Data.Pool
import Database.PostgreSQL.Simple qualified as PG
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HTTP

data FloraJobsEnv = FloraJobsEnv
  { pool :: Pool PG.Connection
  , connectionInfo :: StrictByteString
  , httpManager :: HTTP.Manager
  }
  deriving stock (Generic)
