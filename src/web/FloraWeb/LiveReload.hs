module FloraWeb.LiveReload where

import Data.IORef
import Effectful

import Data.Text (Text)
import Flora.Environment (DeploymentEnv (..))
import FloraWeb.Common.Utils
import Servant.API (Header, Headers, NoContent (..))

livereloadHandler
  :: IOE :> es
  => DeploymentEnv
  -> IORef Bool
  -> Eff es (Headers '[Header "HX-Refresh" Text] NoContent)
livereloadHandler deploymentEnv ioref = do
  case deploymentEnv of
    Development -> do
      needsReload <- liftIO $ readIORef ioref
      if needsReload
        then do
          liftIO $ writeIORef ioref False
          pure $ refresh NoContent
        else pure $ doNotRefresh NoContent
    _ -> pure $ doNotRefresh NoContent
