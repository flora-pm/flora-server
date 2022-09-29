module FloraWeb.Server.Auth.Types where

import Data.Kind (Type)
import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static (Error)
import Effectful.Log (Logging)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (Reader)
import Effectful.Time (Time)
import GHC.Generics
import Servant.API (AuthProtect, Header, Headers)
import Servant.Server
import Servant.Server.Experimental.Auth (AuthServerData)
import Web.Cookie (SetCookie)

import Flora.Model.PersistentSession
import Flora.Model.User
import FloraWeb.Types
import Data.Text (Text)

data Session = Session
  { sessionId :: PersistentSessionId
  , mUser :: Maybe User
  , webEnvStore :: WebEnvStore
  , requestID :: Text
  }
  deriving stock (Generic)

data IsAdmin :: Effect

type instance DispatchOf IsAdmin = Static NoSideEffects
newtype instance StaticRep IsAdmin = IsAdmin ()

runAdminSession
  :: forall (es :: [Effect]) (a :: Type)
   . ()
  => Eff (IsAdmin : es) a
  -> Eff es a
runAdminSession computation = evalStaticRep (IsAdmin ()) computation

data IsVisitor :: Effect

type instance DispatchOf IsVisitor = Static NoSideEffects
newtype instance StaticRep IsVisitor = IsVisitor ()

runVisitorSession
  :: forall (es :: [Effect]) (a :: Type)
   . ()
  => Eff (IsVisitor : es) a
  -> Eff es a
runVisitorSession computation = evalStaticRep (IsVisitor ()) computation

putVisitorTag
  :: forall (es :: [Effect]) (a :: Type)
   . ()
  => Eff es a
  -> Eff (IsVisitor : es) a
putVisitorTag m = raise m

demoteSession
  :: forall (es :: [Effect]) (a :: Type)
   . ()
  => Eff (IsAdmin : es) a
  -> Eff (IsVisitor : es) a
demoteSession = putVisitorTag . runAdminSession

-- | Datatypes used for every route that doesn't *need* an authenticated user
type FloraPage =
  Eff
    '[ IsVisitor
     , DB
     , Time
     , Reader (Headers '[Header "Set-Cookie" SetCookie] Session)
     , Logging
     , Error ServerError
     , IOE
     ]

-- | Datatypes used for routes that *need* an admin
type FloraAdmin =
  Eff
    '[ IsAdmin
     , DB
     , Time
     , Reader (Headers '[Header "Set-Cookie" SetCookie] Session)
     , Logging
     , Error ServerError
     , IOE
     ]

-- | The effect stack for the development websockets
type FloraDevSocket = Eff [Reader (), Logging, Error ServerError, IOE]

type instance
  AuthServerData (AuthProtect "optional-cookie-auth") =
    (Headers '[Header "Set-Cookie" SetCookie] Session)

-- type instance
--   AuthServerData (AuthProtect "cookie-auth") =
--     (Headers '[Header "Set-Cookie" SetCookie] (Session 'Authenticated))
