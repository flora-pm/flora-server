module FloraWeb.Pages.Routes.Categories
  ( Routes
  , Routes' (..)
  )
where

import Data.Text
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid
import Servant.API.Generic

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index :: mode :- Get '[HTML] (Html ())
  , show :: mode :- Capture "category" Text :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)
