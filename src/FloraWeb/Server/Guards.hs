module FloraWeb.Server.Guards where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Entity.DBT (withPool)
import Distribution.Types.Version (Version)
import Flora.Environment (FloraEnv (..))
import Flora.Model.Package
import qualified Flora.Model.Package.Query as Query
import Flora.Model.Release (Release)
import qualified Flora.Model.Release.Query as Query
import FloraWeb.Session (FloraPageM, Session (..), getSession)
import FloraWeb.Templates (defaultTemplateEnv, fromSession)
import FloraWeb.Templates.Error (renderError)
import FloraWeb.Types (fetchFloraEnv)
import Network.HTTP.Types (notFound404)
import Optics.Core

guardThatPackageExists ::
  -- | Namespace
  Namespace ->
  -- | Package name
  PackageName ->
  FloraPageM Package
guardThatPackageExists namespace packageName = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  templateEnv <- fromSession session defaultTemplateEnv
  result <- liftIO $ withPool pool $ Query.getPackageByNamespaceAndName namespace packageName
  case result of
    Nothing -> renderError templateEnv notFound404
    Just package -> pure package

guardThatReleaseExists :: Namespace -> PackageName -> Version -> FloraPageM Release
guardThatReleaseExists namespace packageName version = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  templateEnv <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName
  result <- liftIO $ withPool pool $ Query.getReleaseByVersion (package ^. #packageId) version
  case result of
    Nothing -> renderError templateEnv notFound404
    Just release -> pure release

validateNamespace :: Text -> Maybe Namespace
validateNamespace txt =
  parseNamespace =<< Text.stripPrefix "@" txt
