{-# LANGUAGE PartialTypeSignatures #-}

module FloraWeb.Pages.Server where

import Lucid
import Optics.Core
import Servant

import FloraWeb.Common.Auth
import FloraWeb.Pages.Routes
import FloraWeb.Pages.Server.Admin qualified as Admin
import FloraWeb.Pages.Server.Categories qualified as Categories
import FloraWeb.Pages.Server.Packages qualified as Packages
import FloraWeb.Pages.Server.Search qualified as Search
import FloraWeb.Pages.Server.Sessions qualified as Sessions
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Pages.Home qualified as Home
import FloraWeb.Session
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
