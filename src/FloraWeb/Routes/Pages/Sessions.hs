module FloraWeb.Routes.Pages.Sessions where

type Routes = ToServantApi Routes'

type CreateSession
  = ReqBody '[FormUrlEncoded] LoginForm
  :> Post '[HTML] (Html ())

data Routes' mode = Routes'
  { new    :: mode :- Get '[HTML] (Html ())
  , create :: mode :- CreateSession
  } deriving stock (Generic)

data LoginForm = LoginForm
  { email    :: Text
  , password :: Password
  }
  deriving stock (Generic)
  deriving anyclass FromForm
