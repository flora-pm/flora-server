module FloraWeb.Pages.Routes.Packages
  ( Routes
  , Routes' (..)
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Deriving.Aeson
import Distribution.Types.Version (Version)
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid

import Data.Positive
import Flora.Model.Package.Types
import Servant.API.ContentTypes.GZip

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index
      :: mode
        :- AuthProtect "optional-cookie-auth"
          :> QueryParam "page" (Positive Word)
          :> Get '[HTML] (Html ())
  , showNamespace
      :: mode
        :- AuthProtect "optional-cookie-auth"
          :> Capture "namespace" Namespace
          :> QueryParam "page" (Positive Word)
          :> Get '[HTML] (Html ())
  , showPackage
      :: mode
        :- AuthProtect "optional-cookie-auth"
          :> Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> Get '[HTML] (Html ())
  , showDependents
      :: mode
        :- AuthProtect "optional-cookie-auth"
          :> Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> "dependents"
          :> QueryParam "page" (Positive Word)
          :> QueryParam "q" Text
          :> Get '[HTML] (Html ())
  , showVersionDependents
      :: mode
        :- AuthProtect "optional-cookie-auth"
          :> Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> Capture "version" Version
          :> "dependents"
          :> QueryParam "page" (Positive Word)
          :> QueryParam "q" Text
          :> Get '[HTML] (Html ())
  , showDependencies
      :: mode
        :- AuthProtect "optional-cookie-auth"
          :> Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> "dependencies"
          :> Get '[HTML] (Html ())
  , showVersionDependencies
      :: mode
        :- AuthProtect "optional-cookie-auth"
          :> Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> Capture "version" Version
          :> "dependencies"
          :> Get '[HTML] (Html ())
  , showChangelog
      :: mode
        :- AuthProtect "optional-cookie-auth"
          :> Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> "changelog"
          :> Get '[HTML] (Html ())
  , showVersionChangelog
      :: mode
        :- AuthProtect "optional-cookie-auth"
          :> Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> Capture "version" Version
          :> "changelog"
          :> Get '[HTML] (Html ())
  , showVersion
      :: mode
        :- AuthProtect "optional-cookie-auth"
          :> Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> Capture "version" Version
          :> Get '[HTML] (Html ())
  , listVersions
      :: mode
        :- AuthProtect "optional-cookie-auth"
          :> Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> "versions"
          :> Get '[HTML] (Html ())
  , getTarball
      :: mode
        :- AuthProtect "optional-cookie-auth"
          :> Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> Capture "version" Version
          :> Capture "tarball" Text
          :> Get '[GZipped] ByteString
  , showPackageSecurity
      :: mode
        :- AuthProtect "optional-cookie-auth"
          :> Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> "security"
          :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)
