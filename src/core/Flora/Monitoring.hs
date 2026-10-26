{-# LANGUAGE TemplateHaskell #-}

module Flora.Monitoring
  ( ImportMetrics (..)
  , increaseCounter
  , increasePackageImportCounter
  , registerMetrics
  , registerImportMetrics
  , increaseCounterBy
  , increasePackageImportCounterBy
  , increaseImportFailureCounter
  , setGitHash
  ) where

import Control.Monad (replicateM_)
import Data.Text
import Data.Text qualified as T
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Effectful
import Effectful.Prometheus
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import Prometheus
import Prometheus qualified as P

import Flora.Environment.Env
import Paths_flora (version)

data ImportMetrics = ImportMetrics
  { packageImportCounter :: P.Vector P.Label1 P.Counter
  , packageImportFailureCounter :: P.Vector P.Label2 P.Counter
  }

deriving via
  OnlyCheckWhnfNamed "Flora.Monitoring.ImportMetrics" ImportMetrics
  instance
    NoThunks ImportMetrics

registerImportMetrics :: IOE :> es => Eff es ImportMetrics
registerImportMetrics = do
  packageImportCounter <-
    P.register $
      P.vector "package_index" $
        P.counter
          P.Info
            { metricName = "flora_imported_packages_total"
            , metricHelp = "Packages imported and their index"
            }
  packageImportFailureCounter <-
    P.register $
      P.vector ("package_index", "reason") $
        P.counter
          P.Info
            { metricName = "flora_import_failures_total"
            , metricHelp = "Packages that failed to import, and why"
            }
  pure ImportMetrics{packageImportCounter, packageImportFailureCounter}

registerMetrics :: IOE :> es => Eff es AppMetrics
registerMetrics = do
  buildInformation <-
    P.register $
      P.vector ("git_revision", "version") $
        P.gauge
          P.Info
            { metricName = "build_information"
            , metricHelp = "Build information"
            }
  pure AppMetrics{buildInformation}

setGitHash
  :: Metrics AppMetrics :> es
  => Eff es ()
setGitHash =
  setLabelledGauge (.buildInformation) ($(gitHash), T.pack (showVersion version)) 1.0

increaseCounterBy
  :: Metrics ImportMetrics :> es
  => Int
  -> Text
  -> Eff es ()
increaseCounterBy amount label = do
  replicateM_ amount $ increasePackageImportCounter label

increasePackageImportCounter
  :: Metrics ImportMetrics :> es
  => Text
  -> Eff es ()
increasePackageImportCounter repository = do
  increaseLabelledCounter (.packageImportCounter) repository

increasePackageImportCounterBy
  :: Metrics ImportMetrics :> es
  => Int
  -> Text
  -> Eff es ()
increasePackageImportCounterBy value repository = do
  increaseCounterBy value repository

increaseImportFailureCounter
  :: Metrics ImportMetrics :> es
  => Text
  -> Text
  -> Eff es ()
increaseImportFailureCounter repository reason = do
  increaseLabelledCounter (.packageImportFailureCounter) (repository, reason)
