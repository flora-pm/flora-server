module FloraWeb.Server.Packages
  ( Routes
  , server
  ) where

import Control.Monad.Reader
import Data.Foldable
import Data.Function
import Data.Text
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Orphans ()
import Network.HTTP.Types.Status
import Optics.Core
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid
import Servant.Server.Generic

import Flora.Environment
import Flora.Model.Package
import Flora.Model.Package.Types
import Flora.Model.Release
import Flora.Model.Requirement
import FloraWeb.Server.Auth ()
import FloraWeb.Templates
import FloraWeb.Templates.Error
import qualified FloraWeb.Templates.Pages.Packages as Packages
import FloraWeb.Templates.Types
import FloraWeb.Types

type Routes = ToServantApi Routes'

data Routes' mode = Routes'
  { --new  :: mode :- "new" :> AuthProtect "cookie-auth" :> Get '[HTML] (Html ())
    show :: mode :-  Capture "organisation" Text :> Capture "package" Text :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)

server :: ToServant Routes' (AsServerT FloraM)
server = genericServerT Routes'
  { --new = undefined
    show = showHandler
  }

showHandler :: Text -> Text -> FloraM (Html ())
showHandler namespaceText nameText = do
  FloraEnv{pool} <- ask
  case (validateNamespace namespaceText, validateName nameText) of
    (Just namespace, Just name) -> do
      result <- liftIO $ withPool pool $ getPackageByNamespaceAndName namespace name
      case result of
        Nothing -> renderError notFound404
        Just package -> do
          dependants <- liftIO $ withPool pool $ getPackageDependants namespace name
          releases <- liftIO $ withPool pool $ getReleases (package ^. #packageId)
          let latestRelease =  maximumBy (compare `on` version) releases
          latestReleasedependencies <- liftIO $ withPool pool $ getRequirements (latestRelease ^. #releaseId)
          render emptyAssigns $ Packages.showPackage package dependants releases latestReleasedependencies
    _ -> renderError notFound404

validateNamespace :: Text -> Maybe Namespace
validateNamespace txt =
  parseNamespace =<< stripPrefix "@" txt

validateName :: Text -> Maybe PackageName
validateName txt = parsePackageName txt
