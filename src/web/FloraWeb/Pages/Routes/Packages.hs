module FloraWeb.Pages.Routes.Packages
  ( Routes
  , Routes' (..)
  , PackageFilter (..)
  )
where

import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Distribution.Types.Version (Version)
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid
import Servant.API.Generic
import Servant.API.QueryString

import Data.Positive
import Flora.Model.Package.Types
import Servant.API.ContentTypes.GZip

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { showPackageFeed
      :: mode
        :- AuthProtect "optional-cookie-auth"
          :> "feed"
          :> DeepQuery "filter" PackageFilter
          :> Get '[HTML] (Html ())
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

data PackageFilter = PackageFilter
  { namespace :: Maybe Namespace
  , packageName :: Maybe PackageName
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromDeepQuery PackageFilter where
  fromDeepQuery filterValues =
    let namespaceResult = join $ List.lookup (["namespace" :: Text]) filterValues
        packageNameResult = join $ List.lookup (["package_name"]) filterValues
     in case parseNamespace <$> namespaceResult of
          Nothing -> Left "could not parse namespace"
          Just namespace ->
            case parsePackageName <$> packageNameResult of
              Nothing -> Left "Could not parse package_name"
              Just packageName ->
                Right $ PackageFilter namespace packageName

instance ToDeepQuery PackageFilter where
  toDeepQuery (PackageFilter namespace packageName) =
    [ ([Text.pack "namespace"], Just $ toQueryParam namespace)
    , ([Text.pack "package_name"], Just $ toQueryParam packageName)
    ]
