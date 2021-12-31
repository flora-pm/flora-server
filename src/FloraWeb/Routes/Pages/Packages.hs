module FloraWeb.Routes.Pages.Packages
  ( Routes
  , Routes'(..)
  ) where

import Data.Text
import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid

type Routes = ToServantApi Routes'

data Routes' mode = Routes'
  { show :: mode :-  Capture "organisation" Text :> Capture "package" Text
            :> Get '[HTML] (Html ())
  , showVersion :: mode :- Capture "organisation" Text :> Capture "package" Text
                   :> Capture "version" Text :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)

