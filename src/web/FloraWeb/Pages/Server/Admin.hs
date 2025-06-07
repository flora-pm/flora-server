{-# LANGUAGE PartialTypeSignatures #-}

module FloraWeb.Pages.Server.Admin where

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
import OddJobs.Endpoints qualified as OddJobs
import OddJobs.Types qualified as OddJobs
import Optics.Core
import RequireCallStack
import Servant (HasServer (..), Headers (..))

import Flora.Environment.Env (FeatureEnv (..), FloraEnv (..))
import Flora.Model.Admin.Report
import Flora.Model.Release.Query qualified as Query
import Flora.Model.User
import Flora.Monad
import FloraJobs.Scheduler
import FloraWeb.Common.Auth
import FloraWeb.Common.Utils (handlerToEff, redirect)
import FloraWeb.Pages.Routes.Admin
import FloraWeb.Pages.Templates
  ( ActiveElements (..)
  , TemplateEnv (..)
  , defaultTemplateEnv
  , render
  , templateFromSession
  )
import FloraWeb.Pages.Templates.Admin qualified as Templates
import FloraWeb.Types (RouteEffects, fetchFloraEnv)

server :: RequireCallStack => OddJobs.UIConfig -> OddJobs.Env -> ServerT Routes (Eff RouteEffects)
server cfg env =
  Routes'
    { index = indexHandler
    , oddJobs = \_ -> OddJobs.server cfg env handlerToEff
    , fetchMetadata = fetchMetadataHandler
    }

indexHandler :: SessionWithCookies User -> FloraM RouteEffects (Html ())
indexHandler (Headers session _) = do
  templateEnv <-
    templateFromSession session defaultTemplateEnv
      >>= \te -> pure $ set (#activeElements % #adminDashboard) True te
  FloraEnv{pool} <- liftIO $ fetchFloraEnv session.webEnvStore
  report <- liftIO $ withPool pool getReport
  render templateEnv (Templates.index report)

fetchMetadataHandler :: SessionWithCookies User -> FloraM RouteEffects FetchMetadataResponse
fetchMetadataHandler (Headers session _) = do
  FloraEnv{jobsPool} <- liftIO $ fetchFloraEnv session.webEnvStore

  liftIO $ void $ schedulePackageDeprecationListJob jobsPool

  releasesWithoutReadme <- Query.getHackagePackageReleasesWithoutReadme
  liftIO $
    void $
      forkIO $
        Async.forConcurrently_
          releasesWithoutReadme
          (\(releaseId, version, packagename) -> scheduleReadmeJob jobsPool releaseId packagename version)

  releasesWithoutUploadTime <- Query.getHackagePackageReleasesWithoutUploadTimestamp
  liftIO $
    void $
      forkIO $
        Async.forConcurrently_
          releasesWithoutUploadTime
          (\(releaseId, version, packagename) -> scheduleUploadTimeJob jobsPool releaseId packagename version)

  releasesWithoutChangelog <- Query.getHackagePackageReleasesWithoutChangelog
  liftIO $
    void $
      forkIO $
        Async.forConcurrently_
          releasesWithoutChangelog
          (\(releaseId, version, packagename) -> scheduleChangelogJob jobsPool releaseId packagename version)

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
                scheduleTarballJob jobsPool releaseId packagename version
            )

  packagesWithoutDeprecationInformation <- Query.getHackagePackagesWithoutReleaseDeprecationInformation
  liftIO $
    void $
      forkIO $ do
        Async.forConcurrently_
          packagesWithoutDeprecationInformation
          (\a -> scheduleReleaseDeprecationListJob jobsPool a)
        void $ scheduleRefreshLatestVersions jobsPool

  pure $ redirect "/admin"
