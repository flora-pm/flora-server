{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module FloraWeb.Client where

import Control.Arrow ((>>>))
import Lucid
import Optics.Core
import Servant (AuthProtect, Union)
import Servant.API.Generic
import Servant.Client
import Servant.Client.Core
import Servant.Client.Generic

import Flora.Environment
import Flora.Model.PersistentSession
import Flora.Model.User.Orphans ()
import FloraWeb.Routes as Web
import qualified FloraWeb.Routes.Pages as Web
import qualified FloraWeb.Routes.Pages.Sessions as Web
import FloraWeb.Server.Auth
import Lucid.Orphans ()
import qualified Servant.Client.Core as Client

-- Taken from https://blog.clement.delafargue.name/posts/2019-09-10-a-new-tale-of-servant-clients.html
--
-- While waiting for https://github.com/haskell-servant/servant/issues/1442 these combinators
-- live here.
(//)
  :: (m ~ AsClientT n)
  => GenericServant routes m
  => (a -> ToServant routes m)
  -> (routes m -> b)
  -> (a -> b)
f // f' = f >>> fromServant >>> f'

(/:) :: (a -> b -> c) -> b -> a -> c
(/:) = flip

type instance AuthClientData (AuthProtect "optional-cookie-auth") = Session 'Visitor

floraClient :: (Routes (AsClientT ClientM) -> a) -> a
floraClient = ($ genericClient)

fakeSession :: Session 'Visitor
fakeSession =
  let sessionId = PersistentSessionId $ read "8631b00a-f042-4751-9649-6b0aa617566f"
      mUser = Nothing
      webEnvStore = undefined
   in Session{..}

anonymousRequest :: AuthenticatedRequest (AuthProtect "optional-cookie-auth")
anonymousRequest = mkAuthenticatedRequest fakeSession addSessionCookie

addSessionCookie :: Session p -> Client.Request -> Client.Request
addSessionCookie session req = Client.addHeader "cookie" (session ^. #sessionId) req

createSession :: Web.LoginForm -> ClientM (Union Web.CreateSessionResponses)
createSession loginForm = floraClient $ Web.pages /: anonymousRequest // Web.sessions // Web.create /: loginForm
