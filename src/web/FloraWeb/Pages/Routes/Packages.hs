module FloraWeb.Pages.Routes.Packages
  ( Routes
  , Routes' (..)
  , PackageFilter (..)
  )
where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Deriving.Aeson
import Distribution.Types.Version (Version)
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid
import Text.Atom.Feed qualified as Atom

import Data.Positive
import Flora.Model.Package.Types
import FloraWeb.Atom
import Servant.API.ContentTypes.GZip

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { showPackageFeed
      :: mode
        :- "feed"
          :> QueryParams "packages" PackageFilter
          :> Get '[Atom] Atom.Feed
  , index
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

newtype PackageFilter = PackageFilter
  { selectedPackages :: (Namespace, PackageName)
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] PackageFilter)

instance ToHttpApiData PackageFilter where
  toUrlPiece packages =
    formatSelectedPackage packages.selectedPackages
    where
      formatSelectedPackage :: (Namespace, PackageName) -> Text
      formatSelectedPackage (namespace, packageName) =
        "packages[]=" <> toUrlPiece namespace <> "/" <> toUrlPiece packageName

instance FromHttpApiData PackageFilter where
  parseUrlPiece urlPiece = do
    let (namespace', packageName') = Text.breakOn "/" urlPiece
    namespace <- maybe (Left ("Could not parse namespace " <> namespace')) Right $ parseNamespace namespace'
    packageName <- maybe (Left ("Could not parse package name " <> (Text.tail packageName'))) Right $ parsePackageName (Text.tail packageName')
    pure $ PackageFilter (namespace, packageName)
