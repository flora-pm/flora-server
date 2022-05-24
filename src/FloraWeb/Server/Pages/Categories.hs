module FloraWeb.Server.Pages.Categories where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Lucid (Html)
import Network.HTTP.Types (notFound404)
import Optics.Core ((^.))
import Servant (ServerT)

import Database.PostgreSQL.Entity.DBT
import Flora.Environment (FloraEnv (..))
import qualified Flora.Model.Category.Query as Query
import Flora.Model.Category.Types (Category (..))
import qualified Flora.Model.Package.Query as Query
import FloraWeb.Routes.Pages.Categories
import FloraWeb.Server.Auth
import FloraWeb.Session (getSession)
import FloraWeb.Templates (defaultTemplateEnv, fromSession, render)
import FloraWeb.Templates.Error
import qualified FloraWeb.Templates.Pages.Categories as Template
import FloraWeb.Types (fetchFloraEnv)

server :: ServerT Routes FloraPageM
server =
  Routes'
    { index = indexHandler
    , show = showHandler
    }

indexHandler :: FloraPageM (Html ())
indexHandler = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  templateEnv <- fromSession session defaultTemplateEnv
  categories <- liftIO $ withPool pool Query.getAllCategories
  render templateEnv $ Template.index categories

showHandler :: Text -> FloraPageM (Html ())
showHandler categorySlug = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  templateEnv <- fromSession session defaultTemplateEnv
  result <- liftIO $ withPool pool $ Query.getCategoryBySlug categorySlug
  case result of
    Nothing -> renderError templateEnv notFound404
    Just cat -> do
      packagesInfo <- liftIO $ withPool pool $ Query.getPackagesFromCategoryWithLatestVersion (cat ^. #categoryId)
      render templateEnv $ Template.showCategory cat packagesInfo
