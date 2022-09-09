module FloraWeb.Server.Logging
  ( makeLogger
  , runLog
  , timeAction
  )
where

import Data.Kind (Type)
import Data.Text.Display (display)
import Data.Time.Clock as Time (NominalDiffTime, diffUTCTime)
import Effectful.Log qualified as Log
import Effectful.Time qualified as Time
import Flora.Environment.Config
import Log (Logger, defaultLogLevel)
import Log.Backend.File (FileBackendConfig (..), withJSONFileBackend)

import Effectful
import Effectful.Log (Logging)
import Effectful.Log.Backend.StandardOutput qualified as Log
import Effectful.Time

-- | Wrapper around 'Log.runLogT' with necessary metadata
runLog ::
  forall (es :: [Effect]) (a :: Type).
  (IOE :> es) =>
  DeploymentEnv ->
  Logger ->
  Eff (Logging : es) a ->
  Eff es a
runLog env logger logAction =
  Log.runLogging ("flora-" <> suffix) logger defaultLogLevel logAction
  where
    suffix = display env

makeLogger :: (IOE :> es) => LoggingDestination -> (Logger -> Eff es a) -> Eff es a
makeLogger StdOut = Log.withStdOutLogger
makeLogger Json = Log.withJsonStdOutLogger
makeLogger JSONFile = withJSONFileBackend FileBackendConfig{destinationFile = "logs/flora.json"}

timeAction ::
  forall (es :: [Effect]) (a :: Type).
  (Time :> es) =>
  Eff es a ->
  Eff es (a, NominalDiffTime)
timeAction action = do
  start <- Time.getCurrentTime
  result <- action
  end <- Time.getCurrentTime
  pure (result, Time.diffUTCTime end start)
