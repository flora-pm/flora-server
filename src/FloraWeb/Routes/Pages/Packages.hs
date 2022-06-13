module FloraWeb.Routes.Pages.Packages
  ( Routes
  , Routes' (..)
  )
where

import Distribution.Types.Version (Version)
import Flora.Model.Package (Namespace, PackageName)
import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index ::
      mode
        :- QueryParam "page" Word
          :> Get '[HTML] (Html ())
  , show ::
      mode
        :- Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> Get '[HTML] (Html ())
  , showDependents ::
      mode
        :- Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> "dependents"
          :> Get '[HTML] (Html ())
  , showDependencies ::
      mode
        :- Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> "dependencies"
          :> Get '[HTML] (Html ())
  , showVersion ::
      mode
        :- Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> Capture "version" Version
          :> Get '[HTML] (Html ())
  , listVersions ::
      mode
        :- Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> "versions"
          :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)
