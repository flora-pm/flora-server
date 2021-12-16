module FloraWeb.Server.Auth
  ( module FloraWeb.Server.Auth.Types
  , FloraAuthContext
  , authHandler
  ) where

import Control.Monad.Except
import qualified Data.List as List
import Data.Pool (Pool)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple
import Debug.Trace
import Network.Wai
import Optics.Core
import Servant.API (Header, Headers)
import Servant.Server
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Web.Cookie

import Flora.Environment
import Flora.Model.PersistentSession
import Flora.Model.User
import FloraWeb.Server.Auth.Types
import FloraWeb.Session
import FloraWeb.Types
import Network.HTTP.Types (hCookie)

type FloraAuthContext = AuthHandler Request (Headers '[Header "Set-Cookie" SetCookie] Session)

authHandler :: FloraEnv -> FloraAuthContext
authHandler floraEnv = mkAuthHandler handler
  where
    pool = floraEnv ^. #pool
    handler :: Request -> Handler (Headers '[Header "Set-Cookie" SetCookie] Session)
    handler req = do
      let cookies = traceShowId $ getCookies req
      mbPersistentSessionId <- getSessionId cookies
      mbPersistentSession <- getInTheFuckingSessionShinji pool mbPersistentSessionId
      mUserInfo <- getUser pool mbPersistentSession
      (mUser, sessionId) <- do
        case mUserInfo of
          Nothing -> do
            nSessionId <- liftIO newPersistentSessionId
            pure (Nothing, nSessionId)
          Just (user, userSession) -> do
            pure (Just user, userSession ^. #persistentSessionId)
      webEnvStore <- liftIO $ newWebEnvStore (WebEnv floraEnv)
      let sessionCookie = craftSessionCookie sessionId False
      pure $ addCookie sessionCookie (Session{..})


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
