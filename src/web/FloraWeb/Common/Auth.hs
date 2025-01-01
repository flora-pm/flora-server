module FloraWeb.Common.Auth
  ( module FloraWeb.Common.Auth.Types
  , OptionalAuthContext
  , StrictAuthContext
  , optionalAuthHandler
  , strictAuthHandler
  , adminAuthHandler
  )
where

import Control.Monad.Except qualified as T
import Data.Function ((&))
import Data.Kind (Type)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.PostgreSQL.Transact.Effect qualified as DB
import Log (Logger)
import Network.HTTP.Types (hCookie)
import Network.Wai
import Servant.API (Header, Headers)
import Servant.Server
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Web.Cookie

import Flora.Environment.Env
import Flora.Logging qualified as Logging
import Flora.Model.PersistentSession
import Flora.Model.User
import Flora.Model.User.Query
import FloraWeb.Common.Auth.Types
import FloraWeb.Session
import FloraWeb.Types
import Servant qualified

type OptionalAuthContext = AuthHandler Request (Headers '[Header "Set-Cookie" SetCookie] (Session (Maybe User)))
type StrictAuthContext = AuthHandler Request (Headers '[Header "Set-Cookie" SetCookie] (Session User))

optionalAuthHandler :: Logger -> FloraEnv -> OptionalAuthContext
optionalAuthHandler logger floraEnv =
  mkAuthHandler
    ( \request ->
        handler floraEnv request
          & Logging.runLog floraEnv.environment logger
          & DB.runDB floraEnv.pool
          & effToHandler
    )

strictAuthHandler :: Logger -> FloraEnv -> StrictAuthContext
strictAuthHandler logger floraEnv =
  mkAuthHandler
    ( \request ->
        requireUserHandler floraEnv request
          & Logging.runLog floraEnv.environment logger
          & DB.runDB floraEnv.pool
          & effToHandler
    )

adminAuthHandler :: Logger -> FloraEnv -> StrictAuthContext
adminAuthHandler logger floraEnv =
  mkAuthHandler
    ( \request ->
        requireAdminHandler floraEnv request
          & Logging.runLog floraEnv.environment logger
          & DB.runDB floraEnv.pool
          & effToHandler
    )

requireUserHandler
  :: (DB :> es, Error ServerError :> es, IOE :> es)
  => FloraEnv
  -> Request
  -> Eff es (Headers '[Header "Set-Cookie" SetCookie] (Session User))
requireUserHandler floraEnv req = do
  let cookies = getCookies req
  mbPersistentSessionId <- handlerToEff $ getSessionId cookies
  mbPersistentSession <- getInTheFuckingSessionShinji mbPersistentSessionId
  mUserInfo <- fetchUser mbPersistentSession
  requestID <- liftIO $ getRequestID req
  (user, sessionId) <- do
    case mUserInfo of
      Nothing -> throwError $ err401{errBody = "Log-in first"}
      Just (user, userSession) -> pure (user, userSession.persistentSessionId)
  webEnvStore <- liftIO $ newWebEnvStore (WebEnv floraEnv)
  let sessionCookie = craftSessionCookie sessionId False
  pure $ addCookie sessionCookie (Session{..})

handler
  :: (DB :> es, Error ServerError :> es, IOE :> es)
  => FloraEnv
  -> Request
  -> Eff es (Headers '[Header "Set-Cookie" SetCookie] (Session (Maybe User)))
handler floraEnv req = do
  let cookies = getCookies req
  mbPersistentSessionId <- handlerToEff $ getSessionId cookies
  mbPersistentSession <- getInTheFuckingSessionShinji mbPersistentSessionId
  mUserInfo <- fetchUser mbPersistentSession
  requestID <- liftIO $ getRequestID req
  (user, sessionId) <- do
    case mUserInfo of
      Nothing -> do
        nSessionId <- liftIO newPersistentSessionId
        pure (Nothing, nSessionId)
      Just (user, userSession) -> pure (Just user, userSession.persistentSessionId)
  webEnvStore <- liftIO $ newWebEnvStore (WebEnv floraEnv)
  let sessionCookie = craftSessionCookie sessionId False
  pure $ addCookie sessionCookie (Session{..})

requireAdminHandler
  :: (DB :> es, Error ServerError :> es, IOE :> es)
  => FloraEnv
  -> Request
  -> Eff es (Headers '[Header "Set-Cookie" SetCookie] (Session User))
requireAdminHandler floraEnv req = do
  let cookies = getCookies req
  mbPersistentSessionId <- handlerToEff $ getSessionId cookies
  mbPersistentSession <- getInTheFuckingSessionShinji mbPersistentSessionId
  mUserInfo <- fetchUser mbPersistentSession
  requestID <- liftIO $ getRequestID req
  (user, sessionId) <- do
    case mUserInfo of
      Nothing -> throwError $ err401{errBody = "Log-in first"}
      Just (user, userSession) ->
        if user.userFlags.isAdmin
          then pure (user, userSession.persistentSessionId)
          else throwError $ err404{errBody = "Not Found"}
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
        Just sessionId -> pure (Just sessionId)

getInTheFuckingSessionShinji
  :: DB :> es
  => Maybe PersistentSessionId
  -> Eff es (Maybe PersistentSession)
getInTheFuckingSessionShinji Nothing = pure Nothing
getInTheFuckingSessionShinji (Just persistentSessionId) = do
  result <- getPersistentSession persistentSessionId
  case result of
    Nothing -> pure Nothing
    (Just userSession) -> pure (Just userSession)

fetchUser
  :: (Error ServerError :> es, DB :> es)
  => Maybe PersistentSession
  -> Eff es (Maybe (User, PersistentSession))
fetchUser Nothing = pure Nothing
fetchUser (Just userSession) = do
  user <- lookupUser userSession.userId
  pure (Just (user, userSession))

lookupUser :: (Error ServerError :> es, DB :> es) => UserId -> Eff es User
lookupUser uid = do
  result <- getUserById uid
  case result of
    Nothing -> throwError (err403{errBody = "Invalid Cookie"})
    (Just user) -> pure user

handlerToEff
  :: forall (es :: [Effect]) (a :: Type)
   . Error ServerError :> es
  => Handler a
  -> Eff es a
handlerToEff handler' = do
  v <- unsafeEff_ $ Servant.runHandler handler'
  either throwError pure v

effToHandler
  :: forall (a :: Type)
   . ()
  => Eff '[Error ServerError, IOE] a
  -> Handler a
effToHandler computation = do
  v <- liftIO . runEff . runErrorNoCallStack @ServerError $ computation
  either T.throwError pure v
