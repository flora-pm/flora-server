module FloraWeb.Pages.Server.Categories where

import Data.Text (Text)
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (Reader)
import Lucid (Html)
import Network.HTTP.Types (notFound404)
import Servant (Headers (..), ServerError, ServerT)

import Flora.Environment.Env (FeatureEnv)
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

indexHandler
  :: (DB :> es, IOE :> es, Reader FeatureEnv :> es)
  => SessionWithCookies (Maybe User)
  -> Eff es (Html ())
indexHandler (Headers session _) = do
  templateEnv <- templateFromSession session defaultTemplateEnv
  categories <- Query.getAllCategories
  render templateEnv $ Template.index categories

showHandler
  :: (DB :> es, Error ServerError :> es, IOE :> es, Reader FeatureEnv :> es)
  => SessionWithCookies (Maybe User)
  -> Text
  -> Eff es (Html ())
showHandler (Headers session _) categorySlug = do
  templateEnv <- templateFromSession session defaultTemplateEnv
  result <- Query.getCategoryBySlug categorySlug
  case result of
    Nothing -> renderError templateEnv notFound404
    Just cat -> do
      packagesInfo <- Query.getPackagesFromCategoryWithLatestVersion cat.categoryId
      render templateEnv $ Template.showCategory cat packagesInfo
