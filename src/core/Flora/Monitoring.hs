{-# LANGUAGE TemplateHaskell #-}

module Flora.Monitoring
  ( increaseCounter
  , increasePackageImportCounter
  , registerMetrics
  , increaseCounterBy
  , increasePackageImportCounterBy
  , setGitHash
  , observePoolAcquisition
  , incPoolWaitingThreads
  , decPoolWaitingThreads
  ) where

import Control.Monad (replicateM_)
import Data.Text
import Data.Text qualified as T
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Effectful
import Effectful.Prometheus
import Prometheus as P

import Flora.Environment.Env
import Paths_flora (version)

registerMetrics :: IOE :> es => Eff es AppMetrics
registerMetrics = do
  let packageImportCount =
        P.vector "package_index" $
          P.counter
            P.Info
              { metricName = "flora_imported_packages_total"
              , metricHelp = "Packages imported and their index"
              }
  let gitHashMetric =
        P.vector ("git_revision", "version") $
          P.gauge
            P.Info
              { metricName = "build_information"
              , metricHelp = "Build information"
              }
  let poolAcquisitionTimeMetric =
        P.vector ("pool_name", "flora_instance") $
          P.histogram
            (P.Info "pool_acquisition_duration_seconds" "Wait time to acquire a pool resource")
            [0.01, 0.05, 0.1, 0.5, 1.0, 5.0, 10.0, 30.0, 60.0]
  let poolWaitingThreadsMetrics =
        P.vector ("pool_name", "flora_instance") $
          P.gauge (P.Info "pool_waiting_threads" "Number of threads blocked waiting for a pool resource")
  packageImportCounter <- P.register packageImportCount
  buildInformation <- P.register gitHashMetric
  poolAcquisitionTime <- P.register poolAcquisitionTimeMetric
  poolWaitingThreads <- P.register poolWaitingThreadsMetrics
  pure $ AppMetrics{packageImportCounter, buildInformation, poolAcquisitionTime, poolWaitingThreads}

setGitHash
  :: Metrics AppMetrics :> es
  => Eff es ()
setGitHash =
  setLabelledGauge (.buildInformation) ($(gitHash), T.pack (showVersion version)) 1.0

increaseCounterBy
  :: Metrics AppMetrics :> es
  => Int
  -> Text
  -> Eff es ()
increaseCounterBy amount label = do
  replicateM_ amount $ increasePackageImportCounter label

increasePackageImportCounter
  :: Metrics AppMetrics :> es
  => Text
  -> Eff es ()
increasePackageImportCounter repository = do
  increaseLabelledCounter (.packageImportCounter) repository

increasePackageImportCounterBy
  :: Metrics AppMetrics :> es
  => Int
  -> Text
  -> Eff es ()
increasePackageImportCounterBy value repository = do
  increaseCounterBy value repository

-- | Record how long a connection acquisition took, labelled by pool name and instance.
observePoolAcquisition :: AppMetrics -> Text -> Text -> Double -> IO ()
observePoolAcquisition metrics poolName instanceName seconds =
  P.withLabel metrics.poolAcquisitionTime (poolName, instanceName) (`P.observe` seconds)

-- | Mark a thread as blocked waiting for a pool resource.
incPoolWaitingThreads :: AppMetrics -> Text -> Text -> IO ()
incPoolWaitingThreads metrics poolName instanceName =
  P.withLabel metrics.poolWaitingThreads (poolName, instanceName) P.incGauge

-- | Mark a previously-waiting thread as no longer blocked.
decPoolWaitingThreads :: AppMetrics -> Text -> Text -> IO ()
decPoolWaitingThreads metrics poolName instanceName =
  P.withLabel metrics.poolWaitingThreads (poolName, instanceName) P.decGauge
