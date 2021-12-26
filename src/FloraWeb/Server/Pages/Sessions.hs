module FloraWeb.Server.Pages.Sessions where

import Control.Monad.Reader
import Data.Password.Argon2
import Data.Text
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Optics.Core
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid
import Servant.Server.Generic
import Web.FormUrlEncoded (FromForm)

import Flora.Environment
import Flora.Model.PersistentSession
import Flora.Model.User
import Flora.Model.User.Orphans ()
import FloraWeb.Server.Auth
import FloraWeb.Server.Util
import FloraWeb.Session
import FloraWeb.Templates
import FloraWeb.Templates.Pages.Sessions as Sessions

type Routes = ToServantApi Routes'

type CreateSession
  = ReqBody '[FormUrlEncoded] LoginForm
  :> Post '[HTML] (Html ())

type DeleteSession
  = Capture "session_id" PersistentSessionId
  :> "delete"
  :> Post '[HTML] NoContent

data Routes' mode = Routes'
  { new    :: mode :- Get '[HTML] (Html ())
  , create :: mode :- CreateSession
  , delete :: mode :- DeleteSession
  } deriving stock (Generic)

data LoginForm = LoginForm
  { email    :: Text
  , password :: Password
  , remember :: Bool
  }
  deriving stock (Generic)
  deriving anyclass FromForm

server :: ToServant Routes' (AsServerT FloraPageM)
server = genericServerT Routes'
  { new = newSessionHandler
  , create = createSessionHandler
  , delete = deleteSessionHandler
  }

newSessionHandler :: FloraPageM (Html ())
newSessionHandler = do
  mUser <- asks (\session -> session ^. #mUser)
  case mUser of
    Nothing -> render defaultTemplateEnv Sessions.newSession
    Just _u -> redirect Nothing "/"

createSessionHandler :: LoginForm -> FloraPageM (Html ())
createSessionHandler LoginForm{email, password, remember} = do
  FloraEnv{pool} <- asks (\session -> session ^. #floraEnv )
  mUser <- liftIO $ withPool pool $ getUserByEmail email
  case mUser of
    Nothing -> do
      liftIO $ putStrLn "Couldn't find user"
      let templateEnv = defaultTemplateEnv{flashError = Just (mkError "Could not authenticate!")}
      render templateEnv Sessions.newSession
    Just user ->
      if validatePassword password (user ^. #password)
      then do
        liftIO $ putStrLn "User connected!"
        sessionId <- persistSession pool (user ^. #userId)
        redirect (Just (craftCookie sessionId remember)) "/"
      else do
        liftIO $ putStrLn "Couldn't authenticate user"
        let templateEnv = defaultTemplateEnv{flashError = Just (mkError "Could not authenticate!")}
        render templateEnv Sessions.newSession

deleteSessionHandler :: PersistentSessionId -> FloraPageM NoContent
deleteSessionHandler _sessionId = do
  pure NoContent
