module FloraWeb.Session where

import Data.ByteString
import qualified Data.UUID as UUID
import Flora.Model.PersistentSession
import Web.Cookie
-- | This function builds a cookie with the provided content
craftCookie :: PersistentSessionId -- ^ Cookie content
            -> Bool -- ^ Remember the cookie for 1 week
            -> SetCookie
craftCookie (PersistentSessionId content) rememberSession =
  defaultSetCookie
      { setCookieValue = UUID.toASCIIBytes content
      , setCookieName  = "flora_server_session"
      , setCookieHttpOnly = True
      , setCookieSameSite = Just sameSiteStrict
      , setCookieMaxAge = if rememberSession then Just 604800 else Nothing
      }
