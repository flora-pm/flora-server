{-# LANGUAGE PartialTypeSignatures #-}

module FloraWeb.Pages.Server.Admin where

import Arbiter.Servant qualified as ArbS
import Arbiter.Servant.Server qualified as ArbS
import Arbiter.Servant.UI qualified as ArbUI
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Data.Maybe (isJust)
import Database.PostgreSQL.Entity.DBT
import Effectful (Eff)
import Effectful.Reader.Static (ask)
import Log qualified
import Lucid
import Optics.Core
import RequireCallStack
import Servant (HasServer (..), Headers (..))

import Flora.Database
import Flora.Debug.ThreadDump (forkLabelled, labelledFor_)
import Flora.Environment.Env (FeatureEnv (..), FloraEnv (..))
import Flora.Model.Admin.Report
import Flora.Model.Job
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Model.Release.Query qualified as Query
import Flora.Model.User
import Flora.Monad
import FloraJobs.Scheduler
import FloraWeb.Common.Auth
import FloraWeb.Common.Utils (handlerToEff, redirect)
import FloraWeb.Pages.Routes.Admin
import FloraWeb.Pages.Server.Admin.Groups qualified as Groups
import FloraWeb.Pages.Templates
  ( ActiveElements (..)
  , TemplateEnv (..)
  , defaultTemplateEnv
  , render
  , templateFromSession
  )
import FloraWeb.Pages.Templates.Admin qualified as Templates
import FloraWeb.Types (RouteEffects, fetchFloraEnv)

server
  :: RequireCallStack
  => ArbS.ArbiterServerConfig JobQueues
  -> SessionWithCookies User
  -> ServerT Routes (Eff RouteEffects)
server arbiterUiConfig session =
  Routes'
    { index = indexHandler session
    , arbiterApi = ArbS.arbiterServerHoisted handlerToEff arbiterUiConfig
    , arbiterUi = ArbUI.adminUIServerHoisted handlerToEff
    , fetchMetadata = fetchMetadataHandler session
    , groups = Groups.server session
    }

indexHandler :: SessionWithCookies User -> FloraM RouteEffects (Html ())
indexHandler (Headers session _) = do
  templateEnv <-
    templateFromSession session defaultTemplateEnv
      >>= \te -> pure $ set (#activeElements % #adminDashboard) True te
  FloraEnv{pool} <- liftIO $ fetchFloraEnv session.webEnvStore
  report <- liftIO $ withPool pool getReport
  render templateEnv (Templates.index report)

fetchMetadataHandler :: RequireCallStack => SessionWithCookies User -> FloraM RouteEffects FetchMetadataResponse
fetchMetadataHandler (Headers session _) = do
  FloraEnv{workerEnv, pool} <- liftIO $ fetchFloraEnv session.webEnvStore

  liftIO $ schedulePackageUploadersJob workerEnv
  liftIO $ void $ schedulePackageDeprecationListJob workerEnv

  releasesWithoutReadme <- withReadOnlyPool pool Query.getHackagePackageReleasesWithoutReadme
  liftIO $
    forkLabelled "admin/readme" $
      labelledFor_ "schedule-readme" releasesWithoutReadme $
        \(releaseId, version, packagename) -> scheduleReadmeJob workerEnv releaseId packagename version

  hackageReleasesWithoutUploadInformation <- withReadOnlyPool pool Query.getHackagePackageReleasesWithoutUploadInformation
  liftIO $
    forkLabelled "admin/upload-information" $
      labelledFor_ "schedule-upload-information" hackageReleasesWithoutUploadInformation $
        \(releaseId, version, packagename) -> scheduleUploadInformationJob workerEnv releaseId packagename version

  releasesWithoutChangelog <- withReadOnlyPool pool Query.getHackagePackageReleasesWithoutChangelog
  liftIO $
    forkLabelled "admin/changelog" $
      labelledFor_ "schedule-changelog" releasesWithoutChangelog $
        \(releaseId, version, packagename) -> scheduleChangelogJob workerEnv releaseId packagename version

  features <- ask @FeatureEnv
  when (isJust features.blobStoreImpl) $ do
    releasesWithoutTarball <- withReadOnlyPool pool Query.getHackagePackageReleasesWithoutTarball
    liftIO $
      forkLabelled "admin/tarball" $
        labelledFor_ "schedule-tarball" releasesWithoutTarball $
          \(releaseId, version, packagename) ->
            scheduleTarballJob workerEnv releaseId (Namespace "hackage") packagename version

  packagesWithoutDeprecationInformation <- withReadOnlyPool pool Query.getHackagePackagesWithoutReleaseDeprecationInformation
  liftIO $ forkLabelled "admin/deprecation" $ do
    labelledFor_ "schedule-release-deprecation" packagesWithoutDeprecationInformation $
      scheduleReleaseDeprecationListJob workerEnv
    void $ scheduleRefreshLatestVersions workerEnv

  packagesWithoutMaintainerInformation <- withReadOnlyPool pool Query.getPackagesWithoutMaintainersInformation
  liftIO $
    forkLabelled "admin/maintainers" $
      labelledFor_ "schedule-maintainers" packagesWithoutMaintainerInformation $
        \(_namespace, packageName) -> schedulePackageMaintainersListJob workerEnv packageName

  pure $ redirect "/admin"
