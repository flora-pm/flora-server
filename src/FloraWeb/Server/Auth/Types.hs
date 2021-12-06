module FloraWeb.Server.Auth.Types where

import Control.Monad.Reader (ReaderT)
import Data.Maybe (fromJust)
import GHC.Generics
import Optics.Core
import Servant.API (AuthProtect, Header, Headers)
import Servant.Server
import Servant.Server.Experimental.Auth (AuthServerData)
import Web.Cookie (SetCookie)

import Flora.Model.PersistentSession
import Flora.Model.User
import FloraWeb.Types

type instance AuthServerData (AuthProtect "optional-cookie-auth") = (Headers '[Header "Set-Cookie" SetCookie] (Session 'Visitor))

type instance AuthServerData (AuthProtect "cookie-auth") = (Headers '[Header "Set-Cookie" SetCookie] (Session 'Authenticated))

-- | Datatypes used for every route that doesn't *need* an authenticated user
type FloraPageM = ReaderT (Headers '[Header "Set-Cookie" SetCookie] (Session 'Visitor)) Handler

-- | Datatypes used for routes that *need* an admin
type FloraAdminM = ReaderT (Headers '[Header "Set-Cookie" SetCookie] (Session 'Admin)) Handler

data ProtectionLevel = Visitor | Authenticated | Admin
  deriving stock (Eq, Show, Generic)

data Session (protectionLevel :: ProtectionLevel) = Session
  { sessionId   :: PersistentSessionId
  , mUser       :: Maybe User
  , webEnvStore :: WebEnvStore
  } deriving stock (Generic)

maybeGetUser :: Session 'Visitor -> Maybe User
maybeGetUser session = session ^. #mUser

getUser :: Session 'Authenticated -> User
getUser session = fromJust $ session ^. #mUser

getAdmin :: Session 'Admin -> User
getAdmin session = fromJust $ session ^. #mUser
