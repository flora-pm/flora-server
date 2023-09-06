module FloraWeb.Pages.Server.Categories where

import Data.Text (Text)
import Lucid (Html)
import Network.HTTP.Types (notFound404)
import Servant (ServerT)

import Flora.Model.Category.Query qualified as Query
import Flora.Model.Category.Types (Category (..))
import Flora.Model.Package.Query qualified as Query
import FloraWeb.Common.Auth
import FloraWeb.Pages.Routes.Categories
import FloraWeb.Pages.Templates (defaultTemplateEnv, fromSession, render)
import FloraWeb.Pages.Templates.Error
import FloraWeb.Pages.Templates.Pages.Categories qualified as Template
import FloraWeb.Session (getSession)

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
      packagesInfo <- Query.getPackagesFromCategoryWithLatestVersion (cat.categoryId)
      render templateEnv $ Template.showCategory cat packagesInfo
