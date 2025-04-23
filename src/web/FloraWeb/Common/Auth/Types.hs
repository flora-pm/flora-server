{-# OPTIONS_GHC -Wno-orphans #-}

module FloraWeb.Common.Auth.Types where

import Data.Kind (Type)
import Data.OpenApi ()
import Data.Text (Text)
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader)
import GHC.Generics
import Servant.API (AuthProtect, Header, Headers)
import Servant.Server
import Servant.Server.Experimental.Auth (AuthServerData)
import Web.Cookie (SetCookie)

import Flora.Model.PersistentSession
import Flora.Model.User
import FloraWeb.Types

data Session (a :: Type) = Session
  { sessionId :: PersistentSessionId
  , user :: a
  , webEnvStore :: WebEnvStore
  , requestID :: Text
  }
  deriving stock (Generic)

type SessionWithCookies a =
  (Headers '[Header "Set-Cookie" SetCookie] (Session a))

-- | The effect stack for the development websockets
type FloraDevSocket = Eff [Reader (), Log, Error ServerError, IOE]

type instance
  AuthServerData (AuthProtect "optional-cookie-auth") =
    SessionWithCookies (Maybe User)

type instance
  AuthServerData (AuthProtect "cookie-auth") =
    SessionWithCookies User

type instance
  AuthServerData (AuthProtect "cookie-admin") =
    SessionWithCookies User
