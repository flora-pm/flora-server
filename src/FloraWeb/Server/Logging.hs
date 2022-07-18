module FloraWeb.Server.Logging
  ( alert
  , makeLogger
  , runLog
  )
where

import Data.Aeson.Types (Pair)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Effectful.Log as Log
import Flora.Environment.Config
import Log (LogLevel (..), Logger, defaultLogLevel, object, (.=))

import Data.Text.Display (display)
import Effectful
import Effectful.Log (Logging, logMessageEff')
import qualified Effectful.Log.Backend.StandardOutput as Log
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

alert ::
  ([Time, Logging] :>> es) =>
  Text ->
  [Pair] ->
  Eff es ()
alert message details = do
  timestamp <- getCurrentTime
  let metadata = object $ ("timestamp" .= timestamp) : details
  logMessageEff' LogAttention message metadata
