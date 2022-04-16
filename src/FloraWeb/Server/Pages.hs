{-# LANGUAGE PartialTypeSignatures #-}

module FloraWeb.Server.Pages where

import Lucid
import Optics.Core
import Servant

import FloraWeb.Routes.Pages
import FloraWeb.Server.Auth
import qualified FloraWeb.Server.Pages.Admin as Admin
import qualified FloraWeb.Server.Pages.Categories as Categories
import qualified FloraWeb.Server.Pages.Packages as Packages
import qualified FloraWeb.Server.Pages.Search as Search
import qualified FloraWeb.Server.Pages.Sessions as Sessions
import FloraWeb.Session
import FloraWeb.Templates
import qualified FloraWeb.Templates.Pages.Home as Home

server :: ServerT Routes FloraPageM
server =
  Routes'
    { home = homeHandler
    , about = aboutHandler
    , admin = Admin.server
    , sessions = Sessions.server
    , packages = Packages.server
    , categories = Categories.server
    , search = Search.server
    }

homeHandler :: FloraPageM (Html ())
homeHandler = do
  session <- getSession
  templateDefaults <- fromSession session defaultTemplateEnv
  let templateEnv = templateDefaults & #displayNavbarSearch .~ False
  render templateEnv Home.show

aboutHandler :: FloraPageM (Html ())
aboutHandler = do
  session <- getSession
  templateDefaults <- fromSession session defaultTemplateEnv
  let (templateEnv :: TemplateEnv) =
        templateDefaults
          & #activeElements % #aboutNav .~ True
  render templateEnv Home.about
