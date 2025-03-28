{-# OPTIONS_GHC -Wno-unused-imports #-}

module FloraWeb.Client where

import Data.Proxy (Proxy (..))
import Optics.Core
import Servant (AuthProtect, Union)
import Servant.Client
import Servant.Client.Core
import Servant.Client.Core qualified as Client
import Servant.Client.Generic

import Flora.Model.PersistentSession
import Flora.Model.User
import FloraWeb.Common.Auth
import FloraWeb.Pages.Routes qualified as Pages
import FloraWeb.Pages.Routes qualified as Web
import FloraWeb.Pages.Routes.Sessions qualified as Web
import FloraWeb.Routes as Web
import Lucid.Orphans ()

-- type instance AuthClientData (AuthProtect "optional-cookie-auth") = Session (Maybe User)
--
-- floraClient :: Client ClientM Pages.Routes
-- floraClient = client (Proxy @Pages.Routes)
