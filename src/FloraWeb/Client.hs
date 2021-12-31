{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module FloraWeb.Client where

import Control.Arrow ((>>>))
import Lucid
import Lucid.Orphans ()
import Servant (AuthProtect)
import Servant.API.Generic
import Servant.Client
import Servant.Client.Core
import Servant.Client.Generic

import Flora.Model.PersistentSession
import Flora.Model.User.Orphans ()
import FloraWeb.Routes as FloraWeb
import FloraWeb.Routes.Pages
import FloraWeb.Routes.Pages.Sessions
import FloraWeb.Server.Auth
import Optics.Core
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

type instance AuthClientData (AuthProtect "cookie-auth") = Session

floraClient :: (FloraWeb.Routes (AsClientT ClientM) -> a) -> a
floraClient = ($ genericClient)

fakeSession :: Session
fakeSession =
  let sessionId = PersistentSessionId $ read "8631b00a-f042-4751-9649-6b0aa617566f"
      mUser = Nothing
      floraEnv = undefined
   in Session{..}

anonymousRequest :: AuthenticatedRequest (AuthProtect "cookie-auth")
anonymousRequest = mkAuthenticatedRequest fakeSession addSessionCookie

addSessionCookie :: Session -> Client.Request -> Client.Request
addSessionCookie session req = Client.addHeader "cookie" (session ^. #sessionId) req

createSession :: LoginForm -> ClientM (Html ())
createSession loginInfo = floraClient $ pages /: anonymousRequest  // login // create /: loginInfo
