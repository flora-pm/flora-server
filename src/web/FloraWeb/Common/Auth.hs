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
import Data.Pool
import Data.Text (Text)
import Data.Text.Display
import Data.Text.Encoding qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Simple qualified as PG
import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Log qualified as Log
import Log
import Network.HTTP.Types (hCookie)
import Network.Wai
import RequireCallStack
import Servant qualified
import Servant.API (Header, Headers)
import Servant.Server
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Web.Cookie

import Flora.Database
import Flora.Environment.Env
import Flora.Model.PersistentSession
import Flora.Model.User
import Flora.Model.User.Query
import Flora.Monad
import FloraWeb.Common.Auth.Types
import FloraWeb.Session
import FloraWeb.Types

type OptionalAuthContext = AuthHandler Request (Headers '[Header "Set-Cookie" SetCookie] (Session (Maybe User)))
type StrictAuthContext = AuthHandler Request (Headers '[Header "Set-Cookie" SetCookie] (Session User))

optionalAuthHandler :: RequireCallStack => Logger -> FloraEnv -> OptionalAuthContext
optionalAuthHandler logger floraEnv =
  mkAuthHandler
    ( \request ->
        handler floraEnv request
          & Log.runLog ("flora-server-" <> display floraEnv.environment) logger defaultLogLevel
          & effToHandler
    )

strictAuthHandler :: RequireCallStack => Logger -> FloraEnv -> StrictAuthContext
strictAuthHandler logger floraEnv =
  mkAuthHandler
    ( \request ->
        requireUserHandler floraEnv request
          & Log.runLog ("flora-server-" <> display floraEnv.environment) logger defaultLogLevel
          & effToHandler
    )

adminAuthHandler :: RequireCallStack => Logger -> FloraEnv -> StrictAuthContext
adminAuthHandler logger floraEnv =
  mkAuthHandler
    ( \request ->
        requireAdminHandler floraEnv request
          & Log.runLog ("flora-server-" <> display floraEnv.environment) logger defaultLogLevel
          & effToHandler
    )

requireUserHandler
  :: (Error ServerError :> es, IOE :> es)
  => FloraEnv
  -> Request
  -> FloraM es (Headers '[Header "Set-Cookie" SetCookie] (Session User))
requireUserHandler floraEnv req = do
  let cookies = getCookies req
  mbPersistentSessionId <- handlerToEff $ getSessionId cookies
  mUserInfo <- getInTheFuckingSessionShinji floraEnv.pool mbPersistentSessionId
  requestID <- liftIO $ getRequestID req
  (user, sessionId) <- do
    case mUserInfo of
      Nothing -> throwError $ err401{errBody = "Log-in first"}
      Just (user, userSession) -> pure (user, userSession.persistentSessionId)
  webEnvStore <- liftIO $ newWebEnvStore (WebEnv floraEnv)
  let sessionCookie = craftSessionCookie sessionId False
  pure $ addCookie sessionCookie $ Session sessionId user webEnvStore requestID

handler
  :: (Error ServerError :> es, IOE :> es)
  => FloraEnv
  -> Request
  -> FloraM es (Headers '[Header "Set-Cookie" SetCookie] (Session (Maybe User)))
handler floraEnv req = do
  let cookies = getCookies req
  let theme = getTheme cookies
  mbPersistentSessionId <- handlerToEff $ getSessionId cookies
  mUserInfo <- getInTheFuckingSessionShinji floraEnv.pool mbPersistentSessionId
  requestID <- liftIO $ getRequestID req
  (user, sessionId) <- do
    case mUserInfo of
      Nothing -> do
        nSessionId <- liftIO newPersistentSessionId
        pure (Nothing, nSessionId)
      Just (user, userSession) -> pure (Just user, userSession.persistentSessionId)
  webEnvStore <- liftIO $ newWebEnvStore (WebEnv $ floraEnv{theme = theme})
  let sessionCookie = craftSessionCookie sessionId False
  pure $ addCookie sessionCookie $ Session sessionId user webEnvStore requestID

requireAdminHandler
  :: (Error ServerError :> es, IOE :> es)
  => FloraEnv
  -> Request
  -> FloraM es (Headers '[Header "Set-Cookie" SetCookie] (Session User))
requireAdminHandler floraEnv req = do
  let cookies = getCookies req
  mbPersistentSessionId <- handlerToEff $ getSessionId cookies
  mUserInfo <- getInTheFuckingSessionShinji floraEnv.pool mbPersistentSessionId
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
  pure $ addCookie sessionCookie $ Session sessionId user webEnvStore requestID

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

getTheme :: Cookies -> Maybe Text
getTheme cookies =
  case List.lookup "theme" cookies of
    Nothing -> Nothing
    Just theme -> pure $ Text.decodeUtf8 theme

getSessionId :: Cookies -> Handler (Maybe PersistentSessionId)
getSessionId cookies =
  case List.lookup "flora_server_session" cookies of
    Nothing -> pure Nothing
    Just i ->
      case PersistentSessionId <$> UUID.fromASCIIBytes i of
        Nothing -> pure Nothing
        Just sessionId -> pure (Just sessionId)

-- | Resolve the session and its user in a single read-only transaction, so an
-- authenticated request draws one pooled connection instead of two.
getInTheFuckingSessionShinji
  :: (Error ServerError :> es, IOE :> es)
  => Pool PG.Connection
  -> Maybe PersistentSessionId
  -> FloraM es (Maybe (User, PersistentSession))
getInTheFuckingSessionShinji _ Nothing = pure Nothing
getInTheFuckingSessionShinji pool (Just persistentSessionId) = do
  result <- withReadOnlyPool pool $ do
    mUserSession <- getPersistentSession persistentSessionId
    case mUserSession of
      Nothing -> pure Nothing
      Just userSession -> do
        mUser <- getUserById userSession.userId
        pure (Just (userSession, mUser))
  case result of
    Nothing -> pure Nothing
    Just (_, Nothing) -> throwError (err403{errBody = "Invalid Cookie"})
    Just (userSession, Just user) -> pure (Just (user, userSession))

handlerToEff
  :: forall (es :: [Effect]) (a :: Type)
   . Error ServerError :> es
  => Handler a
  -> FloraM es a
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
