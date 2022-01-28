module FloraWeb.Session where

import Control.Monad.Reader
import Data.Kind
import qualified Data.UUID as UUID
import Servant (Header, Headers, addHeader, getResponse)
import Web.Cookie

import Flora.Model.PersistentSession
import FloraWeb.Server.Auth.Types

getSession :: forall (pLevel :: ProtectionLevel) (hs :: [Type]) (m :: Type -> Type)
           . (MonadReader (Headers hs (Session pLevel)) m) => m (Session pLevel)
getSession = asks getResponse

-- | This function builds a cookie with the provided content
craftSessionCookie :: PersistentSessionId -- ^ Cookie content
                   -> Bool                -- ^ Remember the cookie for 1 week
                   -> SetCookie
craftSessionCookie (PersistentSessionId content) rememberSession =
  defaultSetCookie
      { setCookieValue = UUID.toASCIIBytes content
      , setCookieName  = "flora_server_session"
      , setCookiePath = Just "/"
      , setCookieHttpOnly = True
      , setCookieSameSite = Just sameSiteStrict
      , setCookieMaxAge = if rememberSession then Just 604800 else Nothing
      , setCookieSecure = True
      }

emptySessionCookie :: SetCookie
emptySessionCookie = defaultSetCookie
  { setCookieName = "flora_server_session"
  , setCookieValue = ""
  , setCookieMaxAge = Just 0
  }

addCookie :: SetCookie
          -> a
          -> Headers '[Header "Set-Cookie" SetCookie] a
addCookie cookies continuation = addHeader cookies continuation

deleteCookie :: a -> Headers '[Header "Set-Cookie" SetCookie] a
deleteCookie continuation = addHeader emptySessionCookie continuation
