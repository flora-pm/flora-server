module FloraWeb.Server.Auth.Types where

import Control.Monad.Reader (ReaderT)
import GHC.Generics
import Servant.API (AuthProtect, Header, Headers)
import Servant.Server
import Servant.Server.Experimental.Auth (AuthServerData)

import Flora.Model.PersistentSession
import Flora.Model.User
import FloraWeb.Types
import Web.Cookie (SetCookie)

type instance AuthServerData (AuthProtect "cookie-auth") = (Headers '[Header "Set-Cookie" SetCookie] Session)
-- | Datatypes used for every route that doesn't *need* an authenticated user
type FloraPageM = ReaderT (Headers '[Header "Set-Cookie" SetCookie] Session) Handler

data Session = Session
  { sessionId   :: PersistentSessionId
  , mUser       :: Maybe User
  , webEnvStore :: WebEnvStore
  } deriving stock (Generic)

-- | Datatypes used for routes that *need* an authenticated user
type FloraAdminM = ReaderT (Headers '[Header "Set-Cookie" SetCookie] ProtectedSession) Handler

data ProtectedSession = ProtectedSession
  { sessionId   :: PersistentSessionId
  , user        :: User
  , webEnvStore :: WebEnvStore
  } deriving stock (Generic)
