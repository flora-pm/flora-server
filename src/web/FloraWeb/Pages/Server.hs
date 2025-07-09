{-# LANGUAGE PartialTypeSignatures #-}

module FloraWeb.Pages.Server where

import Lucid
import OddJobs.Endpoints qualified as OddJobs
import OddJobs.Types qualified as OddJobs
import Optics.Core
import RequireCallStack
import Servant

import Flora.Model.User (User)
import FloraWeb.Common.Auth
import FloraWeb.Pages.Routes
import FloraWeb.Pages.Server.Admin qualified as Admin
import FloraWeb.Pages.Server.Categories qualified as Categories
import FloraWeb.Pages.Server.Packages qualified as Packages
import FloraWeb.Pages.Server.Search qualified as Search
import FloraWeb.Pages.Server.Sessions qualified as Sessions
import FloraWeb.Pages.Server.Settings qualified as Settings
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Error (web404)
import FloraWeb.Pages.Templates.Screens.Home qualified as Home
import FloraWeb.Types (FloraEff)

server :: RequireCallStack => OddJobs.UIConfig -> OddJobs.Env -> ServerT Routes FloraEff
server cfg env =
  Routes'
    { home = homeHandler
    , about = aboutHandler
    , admin = Admin.server cfg env
    , sessions = Sessions.server
    , packages = Packages.server
    , categories = Categories.server
    , search = Search.server
    , settings = Settings.server
    , notFound = serveNotFound
    }

homeHandler :: Headers ls (Session (Maybe User)) -> FloraEff (Html ())
homeHandler (Headers session _) = do
  templateDefaults <- templateFromSession session defaultTemplateEnv
  let templateEnv = templateDefaults & #displayNavbarSearch .~ False
  render templateEnv Home.show

aboutHandler :: SessionWithCookies (Maybe User) -> FloraEff (Html ())
aboutHandler (Headers session _) = do
  templateDefaults <- templateFromSession session defaultTemplateEnv
  let (templateEnv :: TemplateEnv) =
        templateDefaults
          & #activeElements
          % #aboutNav
          .~ True
  render templateEnv Home.about

serveNotFound :: RequireCallStack => SessionWithCookies (Maybe User) -> FloraEff (Html ())
serveNotFound (Headers session _) = web404 session
