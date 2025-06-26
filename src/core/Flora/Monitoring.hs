module Flora.Monitoring
  ( increaseCounter
  , increasePackageImportCounter
  , registerMetrics
  , increaseCounterBy
  , increasePackageImportCounterBy
  ) where

import Control.Monad (replicateM_)
import Data.Text
import Effectful
import Effectful.Prometheus
import Prometheus
import Prometheus qualified as P

import Flora.Environment.Env

registerMetrics :: IOE :> es => Eff es AppMetrics
registerMetrics = do
  let packageImportCount =
        P.vector "package_index" $
          P.counter
            P.Info
              { metricName = "flora_imported_packages_total"
              , metricHelp = "Packages imported and their index"
              }
  packageImportCounter <- P.register packageImportCount
  pure $ AppMetrics packageImportCounter

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
  increaseLabelledCounter packageImportCounter repository

increasePackageImportCounterBy
  :: Metrics AppMetrics :> es
  => Int
  -> Text
  -> Eff es ()
increasePackageImportCounterBy value repository = do
  increaseCounterBy value repository
