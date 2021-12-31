module FloraWeb.Routes.Pages.Sessions where

import Data.Password.Argon2
import Data.Text
import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid
import Web.FormUrlEncoded

import Flora.Model.PersistentSession
import Flora.Model.User.Orphans ()

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
