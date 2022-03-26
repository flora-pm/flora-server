module FloraWeb.Server.Logging
  ( alert
  , makeLogger
  , runLog
  ) where

import Data.Aeson.Types (Pair)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Time as Time
import Log (LogT, Logger, defaultLogLevel, object, (.=))
import qualified Log
import qualified Log.Backend.StandardOutput as Log

import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text.Display (display)
import Flora.Environment
import Optics.Core ((^.))
-- | Wrapper around 'Log.runLogT' with necessary metadata
runLog :: forall (m :: Type -> Type) a.
           FloraEnv
        -> Logger
        -> LogT m a
        -> m a
runLog env logger logAction =
  Log.runLogT ("flora-" <> suffix) logger defaultLogLevel logAction
    where
      suffix = display $ env ^. #environment

makeLogger :: LoggingDestination -> (Logger -> IO a) -> IO a
makeLogger StdOut = Log.withStdOutLogger
makeLogger Json   = Log.withJsonStdOutLogger

alert :: (MonadIO m, MonadBase IO m) => Text -> [Pair] -> LogT m ()
alert message details = do
  timestamp <- liftIO Time.getCurrentTime
  let metadata = object $ ("timestamp" .= timestamp) : details
  Log.logAttention message metadata
