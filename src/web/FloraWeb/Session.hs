{-# LANGUAGE AllowAmbiguousTypes #-}

module FloraWeb.Session
  ( module FloraWeb.Common.Auth.Types
  , craftSessionCookie
  , emptySessionCookie
  , getEnv
  , addCookie
  , deleteCookie
  )
where

import Data.UUID qualified as UUID
import Effectful (Eff)
import Effectful.Internal.Monad (unsafeEff_)
import Servant (Header, Headers, addHeader)
import Web.Cookie

import Flora.Environment.Env (FloraEnv)
import Flora.Model.PersistentSession
import FloraWeb.Common.Auth.Types
import FloraWeb.Types (fetchFloraEnv)

getEnv :: Session a -> Eff es FloraEnv
getEnv Session{webEnvStore} = do
  unsafeEff_ $ fetchFloraEnv webEnvStore

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
