module FloraWeb.Pages.Routes.Settings
  ( Routes
  , Routes' (..)
  , TwoFactorSetupResponses
  , TwoFactorConfirmationForm (..)
  , DeleteTwoFactorSetupResponse
  )
where

import Data.Text (Text)
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid
import Servant.API.Generic
import Web.FormUrlEncoded

import FloraWeb.Common.Auth ()

type Routes =
  NamedRoutes Routes'

type GetUserSettings =
  AuthProtect "cookie-auth"
    :> Get '[HTML] (Html ())

type GetUserSecuritySettings =
  AuthProtect "cookie-auth"
    :> "security"
    :> Get '[HTML] (Html ())

type GetTwoFactorSettingsPage =
  AuthProtect "cookie-auth"
    :> "security"
    :> "two-factor"
    :> Get '[HTML] (Html ())

type TwoFactorSetupResponses =
  '[ WithStatus 200 (Html ())
   , WithStatus 301 (Headers '[Header "Location" Text] NoContent)
   ]

data TwoFactorConfirmationForm = TwoFactorConfirmationForm
  { code :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromForm, ToForm)

type PostTwoFactorSetup =
  AuthProtect "cookie-auth"
    :> "security"
    :> "two-factor"
    :> "setup"
    :> ReqBody '[FormUrlEncoded] TwoFactorConfirmationForm
    :> UVerb 'POST '[HTML] TwoFactorSetupResponses

type DeleteTwoFactorSetup =
  AuthProtect "cookie-auth"
    :> "security"
    :> "two-factor"
    :> "delete"
    :> Verb 'POST 301 '[HTML] DeleteTwoFactorSetupResponse

type DeleteTwoFactorSetupResponse =
  Headers '[Header "Location" Text] NoContent

data Routes' mode = Routes'
  { index :: mode :- GetUserSettings
  , getSecuritySettings :: mode :- GetUserSecuritySettings
  , getTwoFactorSettings :: mode :- GetTwoFactorSettingsPage
  , postTwoFactorSetup :: mode :- PostTwoFactorSetup
  , deleteTwoFactorSetup :: mode :- DeleteTwoFactorSetup
  }
  deriving stock (Generic)
