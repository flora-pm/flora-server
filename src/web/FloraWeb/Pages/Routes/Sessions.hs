{-# OPTIONS_GHC -Wno-orphans #-}

module FloraWeb.Pages.Routes.Sessions where

import Data.Text
import Generics.SOP (I (..), NP (..), NS (..))
import Generics.SOP qualified as GSOP
import Lucid
import Servant.API
import Servant.API.ContentTypes.Lucid
import Servant.API.Generic
import Servant.API.MultiVerb
import Web.Cookie
import Web.FormUrlEncoded

import Flora.Model.PersistentSession

instance AsConstructor '[r] (WithHeaders hs r (RespondEmpty code desc)) where
  toConstructor x = I x :* Nil
  fromConstructor (I x :* Nil) = x

type Routes = NamedRoutes Routes'

type NewSession =
  "new"
    :> MultiVerb
         'GET
         '[HTML]
         NewSessionResponses
         NewSessionResult

type NewSessionResponses =
  '[ -- User is already logged-in, redirect to home page
     WithHeaders
       '[Header "Location" Text]
       ((), Text)
       (RespondEmpty 301 "Already logged-in")
   , -- User is not logged-in, dispay the login page
     Respond 200 "Log-in required" (Html ())
   ]

data NewSessionResult
  = AlreadyAuthenticated Text
  | AuthenticationRequired (Html ())
  deriving stock (Generic)

instance GSOP.Generic NewSessionResult

deriving via
  GenericAsUnion NewSessionResponses NewSessionResult
  instance
    AsUnion NewSessionResponses NewSessionResult

-- instance AsUnion NewSessionResponses NewSessionResult where
--   toUnion (AlreadyAuthenticated location) = Z (I ((), location))
--   toUnion (AuthenticationRequired response) = S (Z (I response))
--
--   fromUnion (Z (I ((), location))) = AlreadyAuthenticated location
--   fromUnion (S (Z (I response))) = AuthenticationRequired response
--   fromUnion (S (S x)) = case x of {}

-- instance AsHeaders '[Text, SetCookie] () (Text, SetCookie) where
--   toHeaders (location, cookie) = (I location :* I cookie :* Nil, ())
--   fromHeaders (I location :* I cookie :* Nil, ()) = (location, cookie)

type CreateSession =
  "new"
    :> ReqBody '[FormUrlEncoded] LoginForm
    :> MultiVerb
         'POST
         '[HTML]
         CreateSessionResponses
         CreateSessionResult

type CreateSessionResponses =
  '[ -- Failure, send login page back
     Respond 401 "Authentication failed" (Html ())
   , -- Success, redirected to home page
     WithHeaders
       '[Header "Location" Text, Header "Set-Cookie" SetCookie]
       (Text, SetCookie)
       (RespondEmpty 301 "Authentication succeeded")
   ]

data CreateSessionResult
  = AuthenticationFailure (Html ())
  | AuthenticationSuccess (Text, SetCookie)
  deriving stock (Generic)

instance GSOP.Generic CreateSessionResult

deriving via
  GenericAsUnion CreateSessionResponses CreateSessionResult
  instance
    AsUnion CreateSessionResponses CreateSessionResult

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
