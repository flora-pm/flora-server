module FloraWeb.Server.Logging
  ( makeLogger
  , runLog
  )
where

import Data.Kind (Type)
import qualified Effectful.Log as Log
import Flora.Environment.Config
import Log (Logger, defaultLogLevel)

import Data.Text.Display (display)
import Effectful
import Effectful.Log (Logging)
import qualified Effectful.Log.Backend.StandardOutput as Log
import Log.Backend.File (withJSONFileBackend, FileBackendConfig (..))

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
