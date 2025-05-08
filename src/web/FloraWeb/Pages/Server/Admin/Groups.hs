module FloraWeb.Pages.Server.Admin.Groups where

import FloraWeb.Pages.Templates.Admin.Groups qualified as Templates
import Lucid
import Servant

import Flora.Environment.Env
import Flora.Model.User
import FloraWeb.Common.Auth
import FloraWeb.Types

indexHandler :: SessionWithCookies User -> FloraEff (Html ())
indexHandler (Headers session _) = do
  templateEnv <-
    templateFromSession session defaultTemplateEnv
      >>= \te -> pure $ set (#activeElements % #adminDashboard) True te
  FloraEnv{pool} <- liftIO $ fetchFloraEnv session.webEnvStore
  report <- liftIO $ withPool pool getReport
  render templateEnv (Templates.index report)
