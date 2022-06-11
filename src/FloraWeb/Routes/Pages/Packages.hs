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
  { index :: mode :- Get '[HTML] (Html ())
  , show ::
      mode
        :- Capture' '[Lenient] "namespace" Namespace
          :> Capture' '[Lenient] "package" PackageName
          :> Get '[HTML] (Html ())
  , showDependents ::
      mode
        :- Capture' '[Lenient] "namespace" Namespace
          :> Capture' '[Lenient] "package" PackageName
          :> "dependents"
          :> Get '[HTML] (Html ())
  , showDependencies ::
      mode
        :- Capture' '[Lenient] "namespace" Namespace
          :> Capture' '[Lenient] "package" PackageName
          :> "dependencies"
          :> Get '[HTML] (Html ())
  , showVersion ::
      mode
        :- Capture' '[Lenient] "namespace" Namespace
          :> Capture' '[Lenient] "package" PackageName
          :> Capture' '[Lenient] "version" Version
          :> Get '[HTML] (Html ())
  , listVersions ::
      mode
        :- Capture' '[Lenient] "namespace" Namespace
          :> Capture' '[Lenient] "package" PackageName
          :> "versions"
          :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)
