module FloraWeb.Server.Pages.Categories where

import Data.Text (Text)
import Lucid (Html)
import Network.HTTP.Types (notFound404)
import Optics.Core ((^.))
import Servant (ServerT)

import qualified Flora.Model.Category.Query as Query
import Flora.Model.Category.Types (Category (..))
import qualified Flora.Model.Package.Query as Query
import FloraWeb.Routes.Pages.Categories
import FloraWeb.Server.Auth
import FloraWeb.Session (getSession)
import FloraWeb.Templates (defaultTemplateEnv, fromSession, render)
import FloraWeb.Templates.Error
import qualified FloraWeb.Templates.Pages.Categories as Template

server :: ServerT Routes FloraPage
server =
  Routes'
    { index = indexHandler
    , show = showHandler
    }

indexHandler :: FloraPage (Html ())
indexHandler = do
  session <- getSession
  templateEnv <- fromSession session defaultTemplateEnv
  categories <- Query.getAllCategories
  render templateEnv $ Template.index categories

showHandler :: Text -> FloraPage (Html ())
showHandler categorySlug = do
  session <- getSession
  templateEnv <- fromSession session defaultTemplateEnv
  result <- Query.getCategoryBySlug categorySlug
  case result of
    Nothing -> renderError templateEnv notFound404
    Just cat -> do
      packagesInfo <- Query.getPackagesFromCategoryWithLatestVersion (cat ^. #categoryId)
      render templateEnv $ Template.showCategory cat packagesInfo
