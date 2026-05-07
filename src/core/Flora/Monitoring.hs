{-# LANGUAGE TemplateHaskell #-}

module Flora.Monitoring
  ( increaseCounter
  , increasePackageImportCounter
  , registerMetrics
  , increaseCounterBy
  , increasePackageImportCounterBy
  , setGitHash
  ) where

import Control.Monad (replicateM_)
import Data.Text
import Data.Text qualified as T
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Effectful
import Effectful.Prometheus
import Prometheus
import Prometheus qualified as P

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
  packageImportCounter <- P.register packageImportCount
  gitHashText <- P.register gitHashMetric
  pure $ AppMetrics packageImportCounter gitHashText

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
