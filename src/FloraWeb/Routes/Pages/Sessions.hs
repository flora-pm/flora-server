module FloraWeb.Routes.Pages.Sessions where

import Data.Text
import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid
import Web.FormUrlEncoded

import Flora.Model.PersistentSession
import Flora.Model.User.Orphans ()
import Web.Cookie

type Routes = ToServantApi Routes'

type NewSession
 = "new"
 :> UVerb 'GET '[HTML] NewSessionResponses

type NewSessionResponses
 = '[ WithStatus 200 (Html ()) -- User is not logged-in, dispay the login page
    , WithStatus 301 (Headers '[Header "Location" Text] NoContent)] -- User is already logged-in, redirect to home page

type CreateSession
  = "new"
  :> ReqBody '[FormUrlEncoded] LoginForm
  :> UVerb 'POST '[HTML] CreateSessionResponses

type CreateSessionResponses
 = '[ WithStatus 401 (Html ()) -- Failure, send login page back
    , WithStatus 301 (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)] -- Success, redirected to home page

type DeleteSession
  = "delete"
  :> Capture "session_id" PersistentSessionId
  :> Verb 'POST 301 '[HTML] DeleteSessionResponse

type DeleteSessionResponse
  = Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent

data Routes' mode = Routes'
  { new    :: mode :- NewSession
  , create :: mode :- CreateSession
  , delete :: mode :- DeleteSession
  } deriving stock (Generic)

data LoginForm = LoginForm
  { email    :: Text
  , password :: Text
  , remember :: Maybe ()
  }
  deriving stock (Generic)

instance FromForm LoginForm
instance ToForm LoginForm
