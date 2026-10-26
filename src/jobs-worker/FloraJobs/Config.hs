{-# LANGUAGE BlockArguments #-}

module FloraJobs.Config
  ( FloraJobsConfig (..)
  , JobsMLTP (..)
  , jobsConfigDecoder
  ) where

import Control.DeepSeq
import Data.Text (Text)
import Data.Word
import GHC.Generics
import KDL qualified
import NoThunks.Class (NoThunks)

import Flora.Environment.Config

data JobsMLTP = JobsMLTP
  { sentryDSN :: Maybe String
  , prometheusEnabled :: Bool
  , logger :: LoggingDestination
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData, NoThunks)

data FloraJobsConfig = FloraJobsConfig
  { dbConfig :: PoolConfig
  , connectionInfo :: ConnectionInfo
  , httpPort :: Word16
  , environment :: DeploymentEnv
  , domain :: Text
  , webHttpPort :: Word16
  , mltp :: JobsMLTP
  , features :: FeatureConfig
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData, NoThunks)

jobsConfigDecoder :: KDL.DocumentDecoder FloraJobsConfig
jobsConfigDecoder = KDL.document do
  KDL.nodeWith "jobs-runner" floraJobsConfigDecoder

floraJobsConfigDecoder :: KDL.NodeDecoder FloraJobsConfig
floraJobsConfigDecoder = KDL.children do
  dbConfig <- KDL.nodeWith "pool" poolConfigDecoder
  connectionInfo <- KDL.nodeWith "db" connectionInfoDecoder
  httpPort <- KDL.argAt "httpPort"
  environment <- KDL.argAtWith "environment" deploymentEnvDecoder
  domain <- KDL.argAt "domain"
  webHttpPort <- KDL.argAt "webHttpPort"
  mltp <- KDL.nodeWith "mltp" jobsMltpDecoder
  features <- optionalFeaturesDecoder
  pure FloraJobsConfig{dbConfig, connectionInfo, httpPort, environment, domain, webHttpPort, mltp, features}

jobsMltpDecoder :: KDL.NodeDecoder JobsMLTP
jobsMltpDecoder = KDL.children do
  sentryDSN <- sentryDSNDecoder
  prometheusEnabled <- KDL.argAt "prometheusEnabled"
  logger <- KDL.argAtWith "loggingDestination" loggingDestinationDecoder
  pure JobsMLTP{sentryDSN, prometheusEnabled, logger}
