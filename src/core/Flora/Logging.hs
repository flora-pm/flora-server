module Flora.Logging
  ( makeLogger
  , timeAction
  )
where

import Data.Kind (Type)
import Data.Time.Clock as Time (NominalDiffTime, diffUTCTime)
import Effectful
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Log (Logger)
import Log.Backend.StandardOutput qualified as Log

import Flora.Environment.Config
import Log.Backend.File (FileBackendConfig (..), withJSONFileBackend)

makeLogger :: IOE :> es => LoggingDestination -> (Logger -> Eff es a) -> Eff es a
makeLogger StdOut = Log.withStdOutLogger
makeLogger Json = Log.withJsonStdOutLogger
makeLogger JSONFile = withJSONFileBackend FileBackendConfig{destinationFile = "logs/flora-server.json"}

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
