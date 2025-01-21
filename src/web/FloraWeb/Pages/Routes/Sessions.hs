module FloraWeb.Pages.Routes.Sessions where

import Data.Text
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid
import Servant.API.Generic
import Web.Cookie
import Web.FormUrlEncoded

import Flora.Model.PersistentSession

type Routes = NamedRoutes Routes'

type NewSession =
  "new"
    :> UVerb 'GET '[HTML] NewSessionResponses

type NewSessionResponses =
  '[ -- User is not logged-in, dispay the login page
     WithStatus 200 (Html ())
   , -- User is already logged-in, redirect to home page
     WithStatus 301 (Headers '[Header "Location" Text] NoContent)
   ]

type CreateSession =
  "new"
    :> ReqBody '[FormUrlEncoded] LoginForm
    :> UVerb 'POST '[HTML] CreateSessionResponses

type CreateSessionResponses =
  '[ -- Failure, send login page back
     WithStatus 401 (Html ())
   , -- Success, redirected to home page
     WithStatus 301 (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
   ]

type DeleteSession =
  "delete"
    :> Capture "session_id" PersistentSessionId
    :> Verb 'POST 301 '[HTML] DeleteSessionResponse

type DeleteSessionResponse =
  Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent

data Routes' mode = Routes'
  { new :: mode :- NewSession
  , create :: mode :- CreateSession
  , delete :: mode :- DeleteSession
  }
  deriving stock (Generic)

data LoginForm = LoginForm
  { email :: Text
  , password :: Text
  , totp :: Maybe Text
  }
  deriving stock (Generic)

instance FromForm LoginForm
instance ToForm LoginForm
