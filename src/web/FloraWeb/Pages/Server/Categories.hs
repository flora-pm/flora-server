module FloraWeb.Pages.Server.Categories where

import Data.Text (Text)
import Effectful (IOE, (:>))
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Lucid (Html)
import Network.HTTP.Types (notFound404)
import RequireCallStack
import Servant (Headers (..), ServerError, ServerT)

import Flora.Database
import Flora.Environment.Env (FeatureEnv, FloraEnv (..))
import Flora.Model.Category.Query qualified as Query
import Flora.Model.Category.Types (Category (..))
import Flora.Model.Package.Query qualified as Query
import Flora.Model.User (User)
import Flora.Monad
import FloraWeb.Common.Auth.Types (SessionWithCookies)
import FloraWeb.Pages.Routes.Categories (Routes, Routes' (..))
import FloraWeb.Pages.Templates (TemplateEnv (..), defaultTemplateEnv, render, templateFromSession)
import FloraWeb.Pages.Templates.Error
import FloraWeb.Pages.Templates.Screens.Categories qualified as Template
import FloraWeb.Types (FloraEff)

server :: RequireCallStack => SessionWithCookies (Maybe User) -> ServerT Routes FloraEff
server sessionWithCookies =
  Routes'
    { index = indexHandler sessionWithCookies
    , show = showHandler sessionWithCookies
    }

indexHandler
  :: (IOE :> es, Reader FeatureEnv :> es, Reader FloraEnv :> es)
  => SessionWithCookies (Maybe User)
  -> FloraM es (Html ())
indexHandler (Headers session _) = do
  FloraEnv{pool} <- Reader.ask
  templateEnv' <- templateFromSession session defaultTemplateEnv
  categories <- withReadOnlyPool pool Query.getAllCategories
  packageCount <- withReadOnlyPool pool Query.countPackages
  let templateEnv =
        templateEnv'
          { title = "Categories — Flora.pm"
          , description = "Categories of packages in the Haskell ecosystem"
          }
  render templateEnv $ Template.index packageCount categories

showHandler
  :: ( Error ServerError :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , RequireCallStack
     , Time.Time :> es
     )
  => SessionWithCookies (Maybe User)
  -> Text
  -> FloraM es (Html ())
showHandler (Headers session _) categorySlug = do
  FloraEnv{pool} <- Reader.ask
  templateEnv' <- templateFromSession session defaultTemplateEnv
  now <- Time.currentTime
  result <- withReadOnlyPool pool $ Query.getCategoryBySlug categorySlug
  case result of
    Nothing -> renderError templateEnv' notFound404
    Just cat -> do
      packagesInfo <- withReadOnlyPool pool $ Query.getPackagesFromCategoryWithLatestVersion cat.categoryId
      let templateEnv =
            templateEnv'
              { title = "Categories › " <> cat.name <> " — Flora.pm"
              , description = "Categories of packages in the Haskell ecosystem"
              }
      render templateEnv $ Template.showCategory now cat packagesInfo
