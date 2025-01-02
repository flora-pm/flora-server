module FloraWeb.LiveReload where

import Data.IORef
import Data.Text (Text)
import Effectful
import Servant.API (Header, Headers, NoContent (..))

import Flora.Environment.Env (DeploymentEnv (..))
import FloraWeb.Common.Utils

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
