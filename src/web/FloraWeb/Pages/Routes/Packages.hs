module FloraWeb.Pages.Routes.Packages
  ( Routes
  , Routes' (..)
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Positive
import Data.Text (Text)
import Distribution.Types.Version (Version)
import Flora.Model.Package (Namespace, PackageName)
import Lucid
import Servant
import Servant.API.ContentTypes.GZip
import Servant.API.Generic
import Servant.HTML.Lucid

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index
      :: mode
        :- QueryParam "page" (Positive Word)
          :> Get '[HTML] (Html ())
  , showNamespace
      :: mode
        :- Capture "namespace" Namespace
          :> QueryParam "page" (Positive Word)
          :> Get '[HTML] (Html ())
  , showPackage
      :: mode
        :- Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> Get '[HTML] (Html ())
  , showDependents
      :: mode
        :- Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> "dependents"
          :> QueryParam "page" (Positive Word)
          :> Get '[HTML] (Html ())
  , showVersionDependents
      :: mode
        :- Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> Capture "version" Version
          :> "dependents"
          :> QueryParam "page" (Positive Word)
          :> Get '[HTML] (Html ())
  , showDependencies
      :: mode
        :- Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> "dependencies"
          :> Get '[HTML] (Html ())
  , showVersionDependencies
      :: mode
        :- Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> Capture "version" Version
          :> "dependencies"
          :> Get '[HTML] (Html ())
  , showChangelog
      :: mode
        :- Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> "changelog"
          :> Get '[HTML] (Html ())
  , showVersionChangelog
      :: mode
        :- Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> Capture "version" Version
          :> "changelog"
          :> Get '[HTML] (Html ())
  , showVersion
      :: mode
        :- Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> Capture "version" Version
          :> Get '[HTML] (Html ())
  , listVersions
      :: mode
        :- Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> "versions"
          :> Get '[HTML] (Html ())
  , getTarball
      :: mode
        :- Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> Capture "version" Version
          :> Capture "tarball" Text
          :> Get '[GZipped] ByteString
  }
  deriving stock (Generic)
