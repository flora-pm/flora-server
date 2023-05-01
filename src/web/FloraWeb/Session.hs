{-# LANGUAGE AllowAmbiguousTypes #-}

module FloraWeb.Session
  ( module FloraWeb.Server.Auth.Types
  , getSession
  , getEnv
  , craftSessionCookie
  , emptySessionCookie
  , addCookie
  , deleteCookie
  )
where

import Data.UUID qualified as UUID
import Servant (Header, Headers, addHeader, getResponse)
import Web.Cookie

import Effectful (Eff, type (:>))
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Reader.Static (Reader, asks)
import Flora.Environment
import Flora.Model.PersistentSession
import FloraWeb.Server.Auth.Types
import FloraWeb.Types (fetchFloraEnv)

getSession
  :: Reader (Headers '[Header "Set-Cookie" SetCookie] Session) :> es
  => Eff es Session
getSession = asks (getResponse @'[Header "Set-Cookie" SetCookie])

getEnv :: Reader (Headers '[Header "Set-Cookie" SetCookie] Session) :> es => Eff es FloraEnv
getEnv = do
  Session{webEnvStore} <- getSession
  unsafeEff_ $! fetchFloraEnv webEnvStore

-- | This function builds a cookie with the provided content
craftSessionCookie
  :: PersistentSessionId
  -- ^ Cookie content
  -> Bool
  -- ^ Remember the cookie for 1 week
  -> SetCookie
craftSessionCookie (PersistentSessionId content) rememberSession =
  defaultSetCookie
    { setCookieValue = UUID.toASCIIBytes content
    , setCookieName = "flora_server_session"
    , setCookiePath = Just "/"
    , setCookieHttpOnly = True
    , setCookieSameSite = Just sameSiteStrict
    , setCookieMaxAge = if rememberSession then Just 604800 else Nothing
    , setCookieSecure = True
    }

emptySessionCookie :: SetCookie
emptySessionCookie =
  defaultSetCookie
    { setCookieName = "flora_server_session"
    , setCookieValue = ""
    , setCookieMaxAge = Just 0
    }

addCookie
  :: SetCookie
  -> a
  -> Headers '[Header "Set-Cookie" SetCookie] a
addCookie = addHeader

deleteCookie :: a -> Headers '[Header "Set-Cookie" SetCookie] a
deleteCookie = addHeader emptySessionCookie
