module Flora.Search where

import Data.Text
import Data.Vector (Vector)

import Control.Monad.IO.Class
import Database.PostgreSQL.Entity.DBT (withPool)
import Distribution.Types.Version (Version)
import Flora.Environment (FloraEnv (..))
import Flora.Model.Package (Namespace (..), PackageName)
import qualified Flora.Model.Package.Query as Query
import FloraWeb.Server.Auth (FloraPageM)
import FloraWeb.Session (Session (..), getSession)
import FloraWeb.Types (fetchFloraEnv)
import Optics.Core

-- searchPackageByNamespace :: Namespace -> Text -> FloraPageM (Vector Package)
-- searchPackageByNamespace (Namespace namespace) query = undefined

searchPackageByName :: Text -> FloraPageM (Vector (Namespace, PackageName, Text, Version))
searchPackageByName queryString = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  results <- liftIO $ withPool pool $ Query.searchPackage queryString
  let getInfo = (,,,) <$> view _1 <*> view _2 <*> view _3 <*> view _4
  pure $ fmap getInfo results

listAllPackages :: FloraPageM (Vector (Namespace, PackageName, Text, Version))
listAllPackages = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  results <- liftIO $ withPool pool Query.listAllPackages
  let getInfo = (,,,) <$> view _1 <*> view _2 <*> view _3 <*> view _4
  pure $ fmap getInfo results
