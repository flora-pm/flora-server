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

makeLogger :: IOE :> es => FilePath -> LoggingDestination -> (Logger -> Eff es a) -> Eff es a
makeLogger _ StdOut = Log.withStdOutLogger
makeLogger _ Json = Log.withJsonStdOutLogger
makeLogger jsonFile JSONFile = withJSONFileBackend FileBackendConfig{destinationFile = jsonFile}

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
