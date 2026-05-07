{-# LANGUAGE TemplateHaskell #-}

module FloraJobs.Metrics where

import Data.Text qualified as T
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Effectful
import Effectful.Prometheus
import Prometheus
import Prometheus qualified as P

import Paths_flora (version)

data JobsRunnerMetrics = JobsRunnerMetrics
  { buildInformation :: P.Vector P.Label2 P.Gauge
  }

registerMetrics :: IOE :> es => Eff es JobsRunnerMetrics
registerMetrics = do
  let gitHashMetric =
        P.vector ("git_revision", "version") $
          P.gauge
            P.Info
              { metricName = "build_information"
              , metricHelp = "Build information"
              }
  gitHashText <- P.register gitHashMetric
  pure $ JobsRunnerMetrics gitHashText

setGitHash
  :: Metrics JobsRunnerMetrics :> es
  => Eff es ()
setGitHash =
  setLabelledGauge (.buildInformation) ($(gitHash), T.pack (showVersion version)) 1.0
