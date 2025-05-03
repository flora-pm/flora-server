module FloraWeb.Feed.Routes where

import Data.Text (Text)
import Data.Text qualified as Text
import Deriving.Aeson
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid
import Text.Atom.Feed qualified as Atom
import Web.FormUrlEncoded

import Flora.Model.Package.Types
import FloraWeb.Atom
import FloraWeb.Common.Auth ()

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { feed :: mode :- "atom.xml" :> QueryParams "packages" PackageFilter :> Get '[Atom] Atom.Feed
  , index :: mode :- AuthProtect "optional-cookie-auth" :> Get '[HTML] (Html ())
  , searchPackage
      :: mode
        :- "search"
          :> AuthProtect "optional-cookie-auth"
          :> ReqBody '[FormUrlEncoded] PackageFeedSearchForm
          :> Post '[HTML] (Html ())
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
    packageName <- maybe (Left ("Could not parse package name " <> Text.tail packageName')) Right $ parsePackageName (Text.tail packageName')
    pure $ PackageFilter (namespace, packageName)

newtype PackageFeedSearchForm = PackageFeedSearchForm
  { search :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromForm, ToForm)
