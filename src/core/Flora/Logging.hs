module Flora.Logging
  ( makeLogger
  , runLog
  , timeAction
  )
where

import Data.Kind (Type)
import Data.Text.Display (display)
import Data.Time.Clock as Time (NominalDiffTime, diffUTCTime)
import Effectful
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Log (Logger)
import Log.Backend.File (FileBackendConfig (..), withJSONFileBackend)
import Log.Backend.StandardOutput qualified as Log

import Flora.Environment.Config

-- | Wrapper around 'Log.runLogT' with necessary metadata
runLog
  :: forall (es :: [Effect]) (a :: Type)
   . IOE :> es
  => DeploymentEnv
  -> Logger
  -> Eff (Log : es) a
  -> Eff es a
runLog env logger logAction =
  Log.runLog ("flora-" <> suffix) logger Log.defaultLogLevel logAction
  where
    suffix = display env

makeLogger :: IOE :> es => LoggingDestination -> (Logger -> Eff es a) -> Eff es a
makeLogger StdOut = Log.withStdOutLogger
makeLogger Json = Log.withJsonStdOutLogger
makeLogger JSONFile = withJSONFileBackend FileBackendConfig{destinationFile = "logs/flora.json"}

timeAction
  :: forall (es :: [Effect]) (a :: Type)
   . Time :> es
  => Eff es a
  -> Eff es (a, NominalDiffTime)
timeAction action = do
  start <- Time.currentTime
  result <- action
  end <- Time.currentTime
  pure (result, Time.diffUTCTime end start)
