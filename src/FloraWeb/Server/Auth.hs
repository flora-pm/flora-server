{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module FloraWeb.Server.Auth where

import Control.Monad.Except
import Control.Monad.Reader (ReaderT)
import qualified Data.List as List
import Data.Pool (Pool)
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple
import GHC.Generics
import Network.Wai
import Optics.Core
import Servant.API (AuthProtect)
import Servant.Server
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                         mkAuthHandler)
import Web.Cookie

import qualified Data.UUID as UUID
import Flora.Environment
import Flora.Model.PersistentSession
import Flora.Model.User
import Network.HTTP.Types

type instance AuthServerData (AuthProtect "cookie-auth") = Session

-- | Datatypes used for every route that doesn't *need* an authenticated user
type FloraPageM = ReaderT Session Handler

data Session = Session
  { sessionId :: PersistentSessionId
  , mUser     :: Maybe User
  , floraEnv  :: FloraEnv
  } deriving stock (Show, Generic)

-- | Datatypes used for routes that *need* an authenticated user
type FloraAdminM = ReaderT ProtectedSession Handler

data ProtectedSession = ProtectedSession
  { sessionId :: PersistentSessionId
  , user      :: User
  , floraEnv  :: FloraEnv
  } deriving stock (Generic)

-- | List of cookies used:
--    * "flora_server_session": Used only when a user is registered on the system.
--
-- Process
-- 0) Get the cookies out of the headers
-- 1) Get the "flora_server_session" cookie out of the cookie list
-- 2) Validate the cookie, TTL and signature
-- 3) Get the content of the "flora_server_session" cookie and validate that it is a session_id
-- 4) Fetch the session with this session_id
-- 5) Get the userId out of the session and load the user from the DB
-- 6) Send back a session with a user, a sessionId, and FloraEnv
--
-- ---
--
-- If there is no cookie: Return a session with a new sessionId and no user
-- If the flora_server_session cookie content is not a valid UUID, drop the cookie
-- If fetching the user session fails, return 500
-- If no user session exists for the sessionId, drop the cookie
-- If fetching the user fails, return 500
-- If no user exists with the stored userId, send back 403

authHandler :: FloraEnv -> AuthHandler Request Session
authHandler floraEnv = mkAuthHandler handler
  where
    pool = floraEnv ^. #pool
    handler :: Request -> Handler Session
    handler req = do
      let cookies = getCookies req
      mPersistentSessionId <- getSessionId cookies
      mPersistentSession <- getInTheFuckingSessionShinji pool mPersistentSessionId
      mUserInfo <- getUser pool mPersistentSession
      (mUser, sessionId) <- do
        nSessionId <- liftIO newPersistentSessionId
        case mUserInfo of
          Nothing -> pure (Nothing, nSessionId)
          Just (user, userSession) -> pure (Just user, userSession ^. #persistentSessionId)
      pure Session{..}

getCookies :: Request -> Cookies
getCookies req =
  maybe [] parseCookies (List.lookup hCookie headers)
  where
    headers = requestHeaders req

getSessionId :: Cookies -> Handler (Maybe PersistentSessionId)
getSessionId cookies =
  case List.lookup "flora_server_session" cookies of
    Nothing -> pure Nothing
    Just i ->
      case PersistentSessionId <$> UUID.fromASCIIBytes i of
          Nothing        -> pure Nothing
          Just sessionId -> pure $ Just sessionId

getInTheFuckingSessionShinji :: Pool Connection
                             -> Maybe PersistentSessionId
                             -> Handler (Maybe PersistentSession)
getInTheFuckingSessionShinji _pool Nothing = pure Nothing
getInTheFuckingSessionShinji pool (Just persistentSessionId) = do
  result <- runExceptT $ liftIO $ withPool pool $ getPersistentSession persistentSessionId
  case result of
    Left _                   -> throwError err500
    Right Nothing            -> pure Nothing
    Right (Just userSession) -> pure $ Just userSession

getUser :: Pool Connection -> Maybe PersistentSession -> Handler (Maybe (User, PersistentSession))
getUser _ Nothing = pure Nothing
getUser pool (Just userSession) = do
  user <- lookupUser pool (userSession ^. #userId)
  pure $ Just (user, userSession)

lookupUser :: Pool Connection -> UserId -> Handler User
lookupUser pool uid = do
  result <- runExceptT $ liftIO $ withPool pool $ getUserById uid
  case result of
    Left _            -> throwError (err403 { errBody = "Invalid Cookie" })
    Right Nothing     -> throwError (err403 { errBody = "Invalid Cookie" })
    Right (Just user) -> pure user
