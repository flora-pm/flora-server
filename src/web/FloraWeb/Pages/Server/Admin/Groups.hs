module FloraWeb.Pages.Server.Admin.Groups where

import Lucid
import Optics.Core
import Servant

import Flora.Model.User
import FloraWeb.Common.Auth
import FloraWeb.Pages.Routes.Admin.Groups
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Screens.Admin.Groups qualified as Templates
import FloraWeb.Types

server :: ServerT Routes FloraEff
server =
  Routes'
    { index = indexHandler
    , addGroup = undefined
    , deleteGroup = undefined
    }

indexHandler :: _
indexHandler = undefined
