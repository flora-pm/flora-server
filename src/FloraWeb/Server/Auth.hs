module FloraWeb.Server.Auth
  ( module FloraWeb.Server.Auth.Types
  , FloraAuthContext
  , authHandler
  )
where

import Data.Function ((&))
import Data.List qualified as List
import Data.Text (Text)
import Data.UUID qualified as UUID
import Effectful
import Effectful.Error.Static (Error, throwError)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.PostgreSQL.Transact.Effect qualified as DB
import Effectful.Servant (handlerToEff)
import Effectful.Servant qualified as Servant
import Log (Logger)
import Network.HTTP.Types (hCookie)
import Network.Wai
import Servant.API (Header, Headers)
import Servant.Server
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Web.Cookie

import Data.Text.Encoding qualified as Text
import Data.UUID.V4 qualified as UUID
import Flora.Environment
import Flora.Model.PersistentSession
import Flora.Model.User
import Flora.Model.User.Query
import FloraWeb.Server.Auth.Types
import FloraWeb.Server.Logging qualified as Logging
import FloraWeb.Session
import FloraWeb.Types

type FloraAuthContext = AuthHandler Request (Headers '[Header "Set-Cookie" SetCookie] Session)

authHandler :: Logger -> FloraEnv -> FloraAuthContext
authHandler logger floraEnv =
  mkAuthHandler
    ( \request ->
        handler request
          & Logging.runLog (floraEnv.environment) logger
          & DB.runDB (floraEnv.pool)
          & runVisitorSession
          & Servant.effToHandler
    )
  where
    handler :: Request -> Eff '[Log, DB, IsVisitor, Error ServerError, IOE] (Headers '[Header "Set-Cookie" SetCookie] Session)
    handler req = do
      let cookies = getCookies req
      mbPersistentSessionId <- handlerToEff $ getSessionId cookies
      mbPersistentSession <- getInTheFuckingSessionShinji mbPersistentSessionId
      mUserInfo <- fetchUser mbPersistentSession
      requestID <- liftIO $ getRequestID req
      (mUser, sessionId) <- do
        case mUserInfo of
          Nothing -> do
            nSessionId <- liftIO newPersistentSessionId
            pure (Nothing, nSessionId)
          Just (user, userSession) -> do
            pure (Just user, userSession.persistentSessionId)
      webEnvStore <- liftIO $ newWebEnvStore (WebEnv floraEnv)
      let sessionCookie = craftSessionCookie sessionId False
      pure $ addCookie sessionCookie (Session{..})

getCookies :: Request -> Cookies
getCookies req =
  maybe [] parseCookies (List.lookup hCookie headers)
  where
    headers = requestHeaders req

getRequestID :: Request -> IO Text
getRequestID req = do
  let headers = requestHeaders req
  case List.lookup "X-Request-ID" headers of
    Nothing -> fmap UUID.toText UUID.nextRandom
    Just requestID -> pure $ Text.decodeUtf8 requestID

getSessionId :: Cookies -> Handler (Maybe PersistentSessionId)
getSessionId cookies =
  case List.lookup "flora_server_session" cookies of
    Nothing -> pure Nothing
    Just i ->
      case PersistentSessionId <$> UUID.fromASCIIBytes i of
        Nothing -> pure Nothing
        Just sessionId -> pure $ Just sessionId

getInTheFuckingSessionShinji
  :: (DB :> es)
  => Maybe PersistentSessionId
  -> Eff es (Maybe PersistentSession)
getInTheFuckingSessionShinji Nothing = pure Nothing
getInTheFuckingSessionShinji (Just persistentSessionId) = do
  result <- getPersistentSession persistentSessionId
  case result of
    Nothing -> pure Nothing
    (Just userSession) -> pure $ Just userSession

fetchUser :: (Error ServerError :> es, DB :> es) => Maybe PersistentSession -> Eff es (Maybe (User, PersistentSession))
fetchUser Nothing = pure Nothing
fetchUser (Just userSession) = do
  user <- lookupUser (userSession.userId)
  pure $ Just (user, userSession)

lookupUser :: (Error ServerError :> es, DB :> es) => UserId -> Eff es User
lookupUser uid = do
  result <- getUserById uid
  case result of
    Nothing -> throwError (err403{errBody = "Invalid Cookie"})
    (Just user) -> pure user
