module FloraWeb.Routes.Pages.Packages
  ( Routes
  , Routes'(..)
  , GetRedirect
  ) where

import Data.Text
import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid

type Routes = NamedRoutes Routes'

type GetRedirect
  = Verb 'GET 301 '[HTML] (Headers '[Header "Location" Text] NoContent)

data Routes' mode = Routes'
  { index :: mode :- GetRedirect
  , show  :: mode :-  Capture "organisation" Text
             :> Capture "package" Text
             :> Get '[HTML] (Html ())
  , showVersion :: mode :- Capture "organisation" Text
                   :> Capture "package" Text
                   :> Capture "version" Text :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)

