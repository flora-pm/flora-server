module FloraWeb.Pages.Server.Categories where

import Data.Text (Text)
import Lucid (Html)
import Network.HTTP.Types (notFound404)
import Servant (Headers (..), ServerT)

import Flora.Model.Category.Query qualified as Query
import Flora.Model.Category.Types (Category (..))
import Flora.Model.Package.Query qualified as Query
import Flora.Model.User (User)
import FloraWeb.Common.Auth
import FloraWeb.Pages.Routes.Categories
import FloraWeb.Pages.Templates (defaultTemplateEnv, render, templateFromSession)
import FloraWeb.Pages.Templates.Error
import FloraWeb.Pages.Templates.Screens.Categories qualified as Template
import FloraWeb.Types (FloraEff)

server :: SessionWithCookies (Maybe User) -> ServerT Routes FloraEff
server sessionWithCookies =
  Routes'
    { index = indexHandler sessionWithCookies
    , show = showHandler sessionWithCookies
    }

indexHandler :: SessionWithCookies (Maybe User) -> FloraEff (Html ())
indexHandler (Headers session _) = do
  templateEnv <- templateFromSession session defaultTemplateEnv
  categories <- Query.getAllCategories
  render templateEnv $ Template.index categories

showHandler :: SessionWithCookies (Maybe User) -> Text -> FloraEff (Html ())
showHandler (Headers session _) categorySlug = do
  templateEnv <- templateFromSession session defaultTemplateEnv
  result <- Query.getCategoryBySlug categorySlug
  case result of
    Nothing -> renderError templateEnv notFound404
    Just cat -> do
      packagesInfo <- Query.getPackagesFromCategoryWithLatestVersion cat.categoryId
      render templateEnv $ Template.showCategory cat packagesInfo
