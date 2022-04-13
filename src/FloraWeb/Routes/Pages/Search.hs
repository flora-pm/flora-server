module FloraWeb.Routes.Pages.Search where

import Data.Text (Text)
import Lucid (Html)
import Servant (Get, NamedRoutes, QueryParam, type (:>))
import Servant.API.Generic
import Servant.HTML.Lucid (HTML)

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { displaySearch :: mode :- QueryParam "q" Text :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)
