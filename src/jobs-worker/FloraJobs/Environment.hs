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
import Env (parse)
import GHC.Generics
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

getFloraJobsEnv :: IOE :> es => Eff es FloraJobsEnv
getFloraJobsEnv = do
  jobsConfig <- liftIO $ Env.parse id parseJobsConfig
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
