{-# LANGUAGE ExplicitNamespaces #-}

module FloraWeb.Pages.Routes.Search where

import Data.Positive
import Data.Text (Text)
import Lucid (Html)
import Servant (Get, NamedRoutes, QueryParam, type (:>))
import Servant.API.Generic
import Servant.HTML.Lucid (HTML)

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { displaySearch
      :: mode
        :- QueryParam "q" Text
          :> QueryParam "page" (Positive Word)
          :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)
