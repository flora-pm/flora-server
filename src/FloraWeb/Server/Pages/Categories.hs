module FloraWeb.Server.Pages.Categories where

import Servant (ServerT)
import Data.Text (Text)
import Lucid (Html)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Entity.DBT (withPool)
import Optics.Core ((^.))

import Flora.Environment (FloraEnv(..))
import FloraWeb.Routes.Pages.Categories
import FloraWeb.Server.Auth
import FloraWeb.Session (getSession)
import FloraWeb.Templates (fromSession, defaultTemplateEnv, render)
import FloraWeb.Types (fetchFloraEnv)
import qualified Flora.Model.Category.Query as Query
import qualified FloraWeb.Templates.Pages.Categories as Template

server :: ServerT Routes FloraPageM
server = Routes'
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
showHandler _categorySlug = undefined
  -- session <- getSession
  -- FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  -- templateEnv <- fromSession session defaultTemplateEnv
  -- liftIO $ withPool pool $ Query.getCategoryBySlug categorySlug 
  -- >>= \case
  --   Nothing -> renderError templateEnv notFound404
  --   Just cat -> do
  --     render templateEnv $ Template.showCategory packages
