module FloraWeb.Pages.Server.Security where

import Data.Text.Display (display)
import Effectful (IOE, (:>))
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Lucid (Html)
import Network.HTTP.Types (notFound404)
import RequireCallStack
import Security.Advisories.Core.HsecId (HsecId)
import Servant (Headers (..), ServerError, ServerT)

import Advisories.HsecId.Orphans ()
import Advisories.Model.Advisory.Query qualified as AdvisoryQuery
import Advisories.Model.Advisory.Types (AdvisoryDAO (..))
import Advisories.Model.Affected.Query qualified as Query
import Advisories.Model.Affected.Types (AffectedPackageInfo)
import Data.Positive
import Flora.Database
import Flora.Environment.Env (FeatureEnv, FloraEnv (..))
import Flora.Model.Package.Types (Namespace, PackageName)
import Flora.Model.User (User)
import Flora.Monad
import FloraWeb.Common.Auth.Types (SessionWithCookies)
import FloraWeb.Common.Pagination (fromPage, (?:))
import FloraWeb.Pages.Routes.Security (Routes, Routes' (..))
import FloraWeb.Pages.Templates (TemplateEnv (..), defaultTemplateEnv, render, templateFromSession)
import FloraWeb.Pages.Templates.Error (renderError)
import FloraWeb.Pages.Templates.Screens.Security qualified as Template
import FloraWeb.Types (FloraEff)

server :: RequireCallStack => SessionWithCookies (Maybe User) -> ServerT Routes FloraEff
server sessionWithCookies =
  Routes'
    { listNamespaceAdvisories = listNamespaceAdvisoriesHandler sessionWithCookies
    , listPackageAdvisories = listPackageAdvisoriesHandler sessionWithCookies
    , showAdvisory = showAdvisoryHandler sessionWithCookies
    }

listNamespaceAdvisoriesHandler
  :: ( IOE :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , RequireCallStack
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> Maybe (Positive Word)
  -> FloraM es (Html ())
listNamespaceAdvisoriesHandler (Headers session _) namespace pageParam = do
  FloraEnv{pool} <- Reader.ask
  templateEnv' <- templateFromSession session defaultTemplateEnv
  let pageNumber = pageParam ?: PositiveUnsafe 1
      pagination = fromPage pageNumber
  count <- withReadOnlyPool pool $ Query.countAdvisoriesInNamespace namespace
  results <- withReadOnlyPool pool $ Query.getAdvisoryPreviewsByNamespace pagination namespace
  let templateEnv =
        templateEnv'
          { title = "Security advisories for " <> display namespace <> " — Flora.pm"
          , description = "Security advisories affecting packages in the " <> display namespace <> " namespace"
          }
  render templateEnv $ Template.showNamespaceAdvisories namespace count pageNumber results

listPackageAdvisoriesHandler
  :: ( IOE :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , RequireCallStack
     )
  => SessionWithCookies (Maybe User)
  -> Namespace
  -> PackageName
  -> Maybe (Positive Word)
  -> FloraM es (Html ())
listPackageAdvisoriesHandler (Headers session _) namespace packageName pageParam = do
  FloraEnv{pool} <- Reader.ask
  templateEnv' <- templateFromSession session defaultTemplateEnv
  let pageNumber = pageParam ?: PositiveUnsafe 1
      pagination = fromPage pageNumber
  count <- withReadOnlyPool pool $ Query.countAdvisoriesForPackage namespace packageName
  results <- withReadOnlyPool pool $ Query.getAdvisoryPreviewsByPackage pagination namespace packageName
  let templateEnv =
        templateEnv'
          { title = "Security advisories for " <> display namespace <> "/" <> display packageName <> " — Flora.pm"
          , description = "Security advisories affecting " <> display namespace <> "/" <> display packageName
          }
  render templateEnv $ Template.showPackageAdvisories namespace packageName count pageNumber results

showAdvisoryHandler
  :: ( Error ServerError :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , RequireCallStack
     )
  => SessionWithCookies (Maybe User)
  -> HsecId
  -> FloraM es (Html ())
showAdvisoryHandler (Headers session _) hsecId = do
  FloraEnv{pool} <- Reader.ask
  templateEnv' <- templateFromSession session defaultTemplateEnv
  result <- withReadOnlyPool pool $ AdvisoryQuery.getAdvisoryByHsecId hsecId
  case result of
    Nothing -> renderError templateEnv' notFound404
    Just advisory -> do
      affectedPackages <- withReadOnlyPool pool $ Query.getAffectedPackageInfosByHsecId hsecId
      let templateEnv =
            templateEnv'
              { title = display hsecId <> " — Flora.pm"
              , description = advisory.summary
              }
      render templateEnv $ Template.showAdvisory advisory affectedPackages
