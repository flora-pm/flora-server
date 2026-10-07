module FloraJobs.QueueMetrics
  ( runQueueMetricsLoop
  , collectQueueStats
  ) where

import Arbiter.Core qualified as Arb
import Arbiter.Simple qualified as ArbS
import Control.Monad (forever)
import Data.Aeson (object, (.=))
import Data.Int (Int64)
import Data.Text qualified as Text
import Effectful
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Exception (handleSync)
import Effectful.Log (Log)
import Effectful.Log qualified as Log

import Flora.Model.Job
import FloraJobs.Metrics

refreshInterval :: Int
refreshInterval = 60_000_000

runQueueMetricsLoop
  :: (Concurrent :> es, IOE :> es, Log :> es)
  => ArbS.SimpleEnv JobQueues
  -> JobsRunnerMetrics
  -> Eff es ()
runQueueMetricsLoop workerEnv metrics = forever $ do
  handleSync logFailure $ do
    (stats, deadLettered) <- liftIO $ collectQueueStats workerEnv
    publishQueueStats metrics stats deadLettered
  threadDelay refreshInterval
  where
    logFailure exception =
      Log.logAttention "Could not refresh job queue metrics" $
        object ["error" .= Text.show exception]

collectQueueStats :: ArbS.SimpleEnv JobQueues -> IO (Arb.QueueStats, Int64)
collectQueueStats workerEnv =
  ArbS.runSimpleDb workerEnv $
    (,)
      <$> Arb.getQueueStats @PackageJob @(ArbS.SimpleDb JobQueues IO)
      <*> Arb.countDLQJobs @PackageJob @(ArbS.SimpleDb JobQueues IO)
