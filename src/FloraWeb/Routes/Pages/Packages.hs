module FloraWeb.Routes.Pages.Packages
  ( Routes
  , Routes' (..)
  )
where

import Data.Text
import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index :: mode :- Get '[HTML] (Html ())
  , show ::
      mode
        :- Capture "namespace" Text
          :> Capture "package" Text
          :> Get '[HTML] (Html ())
  , showDependents ::
      mode
        :- Capture "namespace" Text
          :> Capture "package" Text
          :> "dependents"
          :> Get '[HTML] (Html ())
  , showDependencies ::
      mode
        :- Capture "namespace" Text
          :> Capture "package" Text
          :> "dependencies"
          :> Get '[HTML] (Html ())
  , showVersion ::
      mode
        :- Capture "namespace" Text
          :> Capture "package" Text
          :> Capture "version" Text
          :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)
