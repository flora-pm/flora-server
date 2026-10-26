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

import Flora.Environment (featureConfigToEnv, mkPool, readConfigWith)
import Flora.Environment.Config
import Flora.Environment.Domain
import Flora.Environment.Env (FeatureEnv)
import FloraJobs.Config
import FloraJobs.Metrics

deriving via OnlyCheckWhnf HTTP.Manager instance NoThunks HTTP.Manager

data FloraJobsEnv = FloraJobsEnv
  { pool :: Pool PG.Connection
  -- ^ Job handlers and import workers.
  , arbiterPool :: Pool PG.Connection
  -- ^ Arbiter's own statements, see 'arbiterConnections'.
  , dbConfig :: PoolConfig
  , httpManager :: HTTP.Manager
  , httpPort :: Word16
  , metrics :: JobsMetrics
  , environment :: DeploymentEnv
  , mltp :: JobsMLTP
  , features :: FeatureEnv
  , domainEnv :: DomainEnv
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

getFloraJobsEnv :: (Fail :> es, IOE :> es) => FilePath -> Eff es FloraJobsEnv
getFloraJobsEnv configFile = do
  config <- readConfigWith jobsConfigDecoder configFile
  httpManager <- liftIO $ HTTP.newManager tlsManagerSettings
  metrics <- registerMetrics
  let PoolConfig{connectionTimeout} = config.dbConfig
  pool <- mkPool config.connectionInfo connectionTimeout (handlerConnections config.dbConfig)
  arbiterPool <- mkPool config.connectionInfo connectionTimeout arbiterConnections
  features <- featureConfigToEnv config.features
  domainEnv <- mkDomainEnv pool config.dbConfig config.environment config.domain config.webHttpPort
  pure
    FloraJobsEnv
      { pool
      , arbiterPool
      , dbConfig = config.dbConfig
      , httpManager
      , httpPort = config.httpPort
      , metrics
      , environment = config.environment
      , mltp = config.mltp
      , features
      , domainEnv
      }
