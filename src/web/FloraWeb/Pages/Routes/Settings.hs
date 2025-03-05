module FloraWeb.Pages.Routes.Settings
  ( Routes
  , Routes' (..)
  , TwoFactorSetupResponses
  , TwoFactorConfirmationForm (..)
  , DeleteTwoFactorSetupResponse
  , TwoFactorSetupResult (..)
  )
where

import Data.Text (Text)
import Generics.SOP qualified as GSOP
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid
import Servant.API.Generic
import Servant.API.MultiVerb
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
  '[ WithHeaders
       '[Header "Location" Text]
       Text
       (RespondEmpty 301 "2FA Validation Success")
   , WithHeaders
       '[Header "Location" Text]
       Text
       (RespondEmpty 301 "")
   , Respond 400 "2FA Validation Failed" (Html ())
   ]

data TwoFactorSetupResult
  = TwoFactorSetupSuccess Text
  | TwoFactorSetupNotEnabled Text
  | TwoFactorSetupFailure (Html ())
  deriving stock (Generic)
  deriving
    (AsUnion TwoFactorSetupResponses)
    via GenericAsUnion TwoFactorSetupResponses TwoFactorSetupResult

instance GSOP.Generic TwoFactorSetupResult

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
    :> MultiVerb 'POST '[HTML] TwoFactorSetupResponses TwoFactorSetupResult

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
