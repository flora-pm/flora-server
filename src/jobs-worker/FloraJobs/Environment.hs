{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FloraJobs.Environment
  ( FloraJobsEnv (..)
  , getFloraJobsEnv
  ) where

import Data.Pool (Pool)
import Data.Word
import Database.PostgreSQL.Simple qualified as PG
import Effectful
import Effectful.Fail (Fail)
import GHC.Generics
import KDL qualified
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS
import NoThunks.Class (NoThunks, OnlyCheckWhnf (..))

import Flora.Environment (mkPool)
import Flora.Environment.Config
import Flora.Environment.Env ()
import FloraJobs.Metrics

deriving via OnlyCheckWhnf HTTP.Manager instance NoThunks HTTP.Manager

data FloraJobsEnv = FloraJobsEnv
  { pool :: Pool PG.Connection
  , httpManager :: HTTP.Manager
  , httpPort :: Word16
  , metrics :: JobsRunnerMetrics
  , mltp :: MLTP
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

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
    mkPool
      jobsConfig.connectionInfo
      connectionTimeout
      connections
  pure
    FloraJobsEnv
      { pool
      , httpManager
      , httpPort = jobsConfig.jobsHttpPort
      , metrics
      , mltp = jobsConfig.mltp
      }
