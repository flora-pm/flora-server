module FloraWeb.Server.Logging
  ( makeLogger
  , runLog
  )
where

import Data.Kind (Type)
import Effectful.Log qualified as Log
import Flora.Environment.Config
import Log (Logger, defaultLogLevel)

import Data.Text.Display (display)
import Effectful
import Effectful.Log (Logging)
import Effectful.Log.Backend.StandardOutput qualified as Log
import Log.Backend.File (FileBackendConfig (..), withJSONFileBackend)

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
