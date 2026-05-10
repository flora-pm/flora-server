{-# LANGUAGE PartialTypeSignatures #-}

module FloraWeb.Pages.Server.Admin where

import Arbiter.Servant qualified as ArbS
import Arbiter.Servant.Server qualified as ArbS
import Arbiter.Servant.UI qualified as ArbUI
import Control.Concurrent (forkIO)
import Control.Concurrent.Async qualified as Async
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

import Flora.Environment.Env (FeatureEnv (..), FloraEnv (..))
import Flora.Model.Admin.Report
import Flora.Model.Job
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
  FloraEnv{workerEnv} <- liftIO $ fetchFloraEnv session.webEnvStore

  liftIO $ void $ schedulePackageDeprecationListJob workerEnv

  releasesWithoutReadme <- Query.getHackagePackageReleasesWithoutReadme
  liftIO $
    void $
      forkIO $
        Async.forConcurrently_
          releasesWithoutReadme
          (\(releaseId, version, packagename) -> scheduleReadmeJob workerEnv releaseId packagename version)

  hackageReleasesWithoutUploadInformation <- Query.getHackagePackageReleasesWithoutUploadInformation
  liftIO $
    void $
      forkIO $
        Async.forConcurrently_
          hackageReleasesWithoutUploadInformation
          (\(releaseId, version, packagename) -> scheduleUploadInformationJob workerEnv releaseId packagename version)

  releasesWithoutChangelog <- Query.getHackagePackageReleasesWithoutChangelog
  liftIO $
    void $
      forkIO $
        Async.forConcurrently_
          releasesWithoutChangelog
          (\(releaseId, version, packagename) -> scheduleChangelogJob workerEnv releaseId packagename version)

  features <- ask @FeatureEnv
  Log.logAttention "features" features
  when (isJust features.blobStoreImpl) $ do
    releasesWithoutTarball <- Query.getHackagePackageReleasesWithoutTarball
    liftIO $!
      void $!
        forkIO $!
          Async.forConcurrently_
            releasesWithoutTarball
            ( \(releaseId, version, packagename) ->
                scheduleTarballJob workerEnv releaseId packagename version
            )

  packagesWithoutDeprecationInformation <- Query.getHackagePackagesWithoutReleaseDeprecationInformation
  liftIO $
    void $
      forkIO $ do
        Async.forConcurrently_
          packagesWithoutDeprecationInformation
          (\a -> scheduleReleaseDeprecationListJob workerEnv a)
        void $ scheduleRefreshLatestVersions workerEnv

  pure $ redirect "/admin"
