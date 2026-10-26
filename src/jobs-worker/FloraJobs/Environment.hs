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
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS
import NoThunks.Class (NoThunks, OnlyCheckWhnf (..))

import Flora.Environment (mkPool, readFloraConfig)
import Flora.Environment.Config
import Flora.Environment.Env ()
import FloraJobs.Metrics

deriving via OnlyCheckWhnf HTTP.Manager instance NoThunks HTTP.Manager

data FloraJobsEnv = FloraJobsEnv
  { pool :: Pool PG.Connection
  -- ^ Job handlers and import workers.
  , arbiterPool :: Pool PG.Connection
  -- ^ Arbiter's own statements, see 'arbiterConnections'.
  , httpManager :: HTTP.Manager
  , httpPort :: Word16
  , metrics :: JobsRunnerMetrics
  , config :: FloraConfig
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

getFloraJobsEnv :: (Fail :> es, IOE :> es) => FilePath -> Eff es FloraJobsEnv
getFloraJobsEnv config = do
  jobsConfig <- readFloraConfig config
  httpManager <- liftIO $ HTTP.newManager tlsManagerSettings
  metrics <- registerMetrics
  let PoolConfig{connectionTimeout} = jobsConfig.dbConfig
  pool <- mkPool jobsConfig.connectionInfo connectionTimeout (handlerConnections jobsConfig.dbConfig)
  arbiterPool <- mkPool jobsConfig.connectionInfo connectionTimeout arbiterConnections
  pure
    FloraJobsEnv
      { pool
      , arbiterPool
      , httpManager
      , httpPort = jobsConfig.jobsHttpPort
      , metrics
      , config = jobsConfig
      }
