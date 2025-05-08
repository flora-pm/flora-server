module FloraWeb.Pages.Server.Admin.Groups where
import FloraWeb.Common.Auth
import Flora.Model.User
import FloraWeb.Types
import Lucid
import Flora.Environment.Env
import Servant
import Servant


indexHandler :: SessionWithCookies User -> FloraEff (Html ())
indexHandler (Headers session _) = do
  templateEnv <-
    templateFromSession session defaultTemplateEnv
      >>= \te -> pure $ set (#activeElements % #adminDashboard) True te
  FloraEnv{pool} <- liftIO $ fetchFloraEnv session.webEnvStore
  report <- liftIO $ withPool pool getReport
  render templateEnv (Templates.index report)
