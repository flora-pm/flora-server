{-# LANGUAGE PartialTypeSignatures #-}

module FloraWeb.Server.Pages where

import Lucid
import Optics.Core
import Servant

import FloraWeb.Routes.Pages
import FloraWeb.Server.Auth
import FloraWeb.Server.Pages.Admin qualified as Admin
import FloraWeb.Server.Pages.Categories qualified as Categories
import FloraWeb.Server.Pages.Packages qualified as Packages
import FloraWeb.Server.Pages.Search qualified as Search
import FloraWeb.Server.Pages.Sessions qualified as Sessions
import FloraWeb.Session
import FloraWeb.Templates
import FloraWeb.Templates.Pages.Home qualified as Home
import OddJobs.Endpoints qualified as OddJobs
import OddJobs.Types qualified as OddJobs

server :: OddJobs.UIConfig -> OddJobs.Env -> ServerT Routes FloraPage
server cfg env =
  Routes'
    { home = homeHandler
    , about = aboutHandler
    , admin = Admin.server cfg env
    , sessions = Sessions.server
    , packages = Packages.server
    , categories = Categories.server
    , search = Search.server
    }

homeHandler :: FloraPage (Html ())
homeHandler = do
  session <- getSession
  templateDefaults <- fromSession session defaultTemplateEnv
  let templateEnv = templateDefaults & #displayNavbarSearch .~ False
  render templateEnv Home.show

aboutHandler :: FloraPage (Html ())
aboutHandler = do
  session <- getSession
  templateDefaults <- fromSession session defaultTemplateEnv
  let (templateEnv :: TemplateEnv) =
        templateDefaults
          & #activeElements % #aboutNav .~ True
  render templateEnv Home.about
