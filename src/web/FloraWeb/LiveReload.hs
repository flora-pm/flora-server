module FloraWeb.LiveReload
  ( ReloadEvent (..)
  , liveReloadHandler
  , watchAssets
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, void, when)
import Effectful
import Servant.API.EventStream (ToServerEvent (..), commentEvent, serverEvent)
import Servant.API.Stream (SourceIO)
import Servant.Types.SourceT (fromAction, source)
import System.Directory (createDirectoryIfMissing)
import System.FSNotify qualified as FSNotify
import System.Timeout (timeout)

import Flora.Environment.Config (DeploymentEnv (..))

heartbeatIntervalMicros :: Int
heartbeatIntervalMicros = 2_000_000

debounceMicros :: Int
debounceMicros = 200_000

data ReloadEvent = Reload | Heartbeat

instance ToServerEvent ReloadEvent where
  toServerEvent Reload = serverEvent (Just "reload") Nothing "reload"
  toServerEvent Heartbeat = commentEvent "keepalive"

liveReloadHandler
  :: IOE :> es
  => DeploymentEnv
  -> TChan ()
  -> Eff es (SourceIO ReloadEvent)
liveReloadHandler environment broadcastChan =
  case environment of
    Development -> do
      clientChan <- liftIO $ atomically $ dupTChan broadcastChan
      pure $ reloadStream clientChan
    _ -> pure $ source []

reloadStream :: TChan () -> SourceIO ReloadEvent
reloadStream clientChan = fromAction (const False) nextEvent
  where
    nextEvent :: IO ReloadEvent
    nextEvent = do
      result <- timeout heartbeatIntervalMicros $ atomically $ readTChan clientChan
      pure $ maybe Heartbeat (const Reload) result

watchAssets :: FilePath -> TChan () -> IO ()
watchAssets directory broadcastChan = do
  createDirectoryIfMissing True directory
  FSNotify.withManager $ \manager -> do
    dirtyVar <- newTVarIO False
    void $
      FSNotify.watchTree
        manager
        directory
        (const True)
        (\_event -> atomically $ writeTVar dirtyVar True)
    let settle = do
          atomically $ writeTVar dirtyVar False
          threadDelay debounceMicros
          dirtied <- readTVarIO dirtyVar
          when dirtied settle
    forever $ do
      atomically $ check =<< readTVar dirtyVar
      settle
      atomically $ writeTChan broadcastChan ()
