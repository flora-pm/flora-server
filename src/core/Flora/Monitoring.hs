module Flora.Monitoring
  ( increaseCounter
  , increasePackageImportCounter
  , registerMetrics
  , increaseCounterBy
  , increasePackageImportCounterBy
  ) where

import Control.Monad (void, when)
import Data.Text
import Effectful
import Effectful.Reader.Static (Reader, ask)
import GHC.Records
import Prometheus
import Prometheus qualified as P

import Flora.Environment.Env

registerMetrics :: IOE :> es => Eff es Metrics
registerMetrics = do
  let packageImportCount =
        P.vector "package_index" $
          P.counter
            P.Info
              { metricName = "flora_packages_imported_total"
              , metricHelp = "Packages imported and their index"
              }
  packageImportCounter <- P.register packageImportCount
  pure Metrics{..}

increaseCounter
  :: forall r es label
   . ( HasField "mltp" r MLTP
     , Reader r :> es
     , Label label
     , IOE :> es
     )
  => Vector label Counter
  -> label
  -> Eff es ()
increaseCounter promVector label = do
  env <- ask
  let mltpConf = env.mltp
  when mltpConf.prometheusEnabled $
    liftIO $
      withLabel promVector label incCounter

increaseCounterBy
  :: ( HasField "mltp" r MLTP
     , Reader r :> es
     , Label label
     , IOE :> es
     )
  => Double
  -> Vector label Counter
  -> label
  -> Eff es ()
increaseCounterBy value promVector label = do
  env <- ask
  let mltpConf = env.mltp
  when mltpConf.prometheusEnabled $
    liftIO $
      withLabel promVector label (\c -> void $ addCounter c value)

increasePackageImportCounter
  :: ( HasField "metrics" r Metrics
     , HasField "mltp" r MLTP
     , Reader r :> es
     , IOE :> es
     )
  => Text
  -> Eff es ()
increasePackageImportCounter repository = do
  env <- ask
  increaseCounter env.metrics.packageImportCounter repository

increasePackageImportCounterBy
  :: ( HasField "metrics" r Metrics
     , HasField "mltp" r MLTP
     , Reader r :> es
     , IOE :> es
     )
  => Double
  -> Text
  -> Eff es ()
increasePackageImportCounterBy value repository = do
  env <- ask
  increaseCounterBy value env.metrics.packageImportCounter repository
