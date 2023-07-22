{-# OPTIONS_GHC -Wno-unused-imports #-}

module FloraWeb.Client where

import Optics.Core
import Servant (AuthProtect, Union)
import Servant.Client
import Servant.Client.Core
import Servant.Client.Generic

import Flora.Model.PersistentSession
import Flora.Model.User.Orphans ()
import FloraWeb.Common.Auth
import FloraWeb.Pages.Routes qualified as Pages
import FloraWeb.Pages.Routes qualified as Web
import FloraWeb.Pages.Routes.Sessions qualified as Web
import FloraWeb.Routes as Web
import Lucid.Orphans ()
import Servant.Client.Core qualified as Client

-- type instance AuthClientData (AuthProtect "optional-cookie-auth") = Session 'Visitor

-- floraClient :: Client ClientM Pages.Routes
-- floraClient = genericClient

-- fakeSession :: Session 'Visitor
-- fakeSession =
--   let sessionId = PersistentSessionId $! read "8631b00a-f042-4751-9649-6b0aa617566f"
--       mUser = Nothing
--       webEnvStore = undefined
--    in Session{..}

-- anonymousRequest :: AuthenticatedRequest (AuthProtect "optional-cookie-auth")
-- anonymousRequest = mkAuthenticatedRequest fakeSession addSessionCookie

-- addSessionCookie :: Session p -> Client.Request -> Client.Request
-- addSessionCookie session req = Client.addHeader "cookie" (session.sessionId) req

-- createSession :: Web.LoginForm -> ClientM (Union Web.CreateSessionResponses)
-- createSession loginForm =
--   floraClient // Web.pages /: anonymousRequest // Web.sessions // Web.create /: loginForm
