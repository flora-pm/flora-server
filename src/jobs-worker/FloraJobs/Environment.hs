module FloraJobs.Environment
  ( FloraJobsEnv (..)
  , getFloraJobsEnv
  ) where

import Data.ByteString (StrictByteString)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Word
import Database.PostgreSQL.Simple qualified as PG
import Effectful
import Effectful.Fail (Fail)
import GHC.Generics
import KDL qualified
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS

import Flora.Environment.Config
import FloraJobs.Metrics

data FloraJobsEnv = FloraJobsEnv
  { pool :: Pool PG.Connection
  , connectionInfo :: StrictByteString
  , httpManager :: HTTP.Manager
  , httpPort :: Word16
  , metrics :: JobsRunnerMetrics
  , mltp :: MLTP
  }
  deriving stock (Generic)

getFloraJobsEnv :: (Fail :> es, IOE :> es) => FilePath -> Eff es FloraJobsEnv
getFloraJobsEnv config = do
  jobsConfig <-
    liftIO (KDL.decodeFileWith floraEnvDecoder config) >>= \case
      Right env -> pure env
      Left e -> fail $ show e
  httpManager <- liftIO $ HTTP.newManager tlsManagerSettings
  metrics <- registerMetrics
  let PoolConfig{connectionTimeout, connections} = jobsConfig.dbConfig
  pool <-
    liftIO $
      Pool.newPool $
        Pool.defaultPoolConfig
          (PG.connectPostgreSQL jobsConfig.connectionInfo)
          PG.close
          (realToFrac connectionTimeout)
          connections
  pure
    FloraJobsEnv
      { pool
      , connectionInfo = jobsConfig.connectionInfo
      , httpManager
      , httpPort = jobsConfig.httpPort
      , metrics
      , mltp = jobsConfig.mltp
      }
