module FloraWeb.Server.Pages.Admin where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async qualified as Async
import Control.Monad.IO.Class
import Data.Proxy (Proxy (..))
import Database.PostgreSQL.Entity.DBT
import Lucid
import Network.HTTP.Types.Status (notFound404)
import OddJobs.Endpoints qualified as OddJobs
import OddJobs.Types qualified as OddJobs
import Optics.Core
import Servant (HasServer (..), hoistServer)

import Control.Monad (void)
import Flora.Environment (FloraEnv (..))
import Flora.Model.Admin.Report
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Release.Query qualified as Query
import Flora.Model.User
import Flora.Model.User.Query qualified as Query
import FloraJobs.Scheduler
import FloraWeb.Routes.Pages.Admin
import FloraWeb.Server.Auth
import FloraWeb.Server.Utils (handlerToEff, redirect)
import FloraWeb.Session (getSession)
import FloraWeb.Templates (ActiveElements (..), TemplateEnv (..), defaultTemplateEnv, fromSession, render)
import FloraWeb.Templates.Admin qualified as Templates
import FloraWeb.Templates.Admin.Packages qualified as Templates
import FloraWeb.Templates.Admin.Users qualified as Templates
import FloraWeb.Templates.Error
import FloraWeb.Types (fetchFloraEnv)

server :: OddJobs.UIConfig -> OddJobs.Env -> ServerT Routes FloraPage
server cfg env =
  ensureAdmin $
    Routes'
      { index = indexHandler
      , users = adminUsersHandler
      , packages = adminPackagesHandler
      , oddJobs = OddJobs.server cfg env handlerToEff
      , fetchMetadata = fetchMetadataHandler
      , importIndex = indexImportJobHandler
      }

-- | This function converts a sub-tree of routes that require 'Admin' role
--  to a sub-tree of Flora pages.
--  It acts as the safeguard that rejects non-admins from protected routes.
ensureAdmin :: ServerT Routes FloraAdmin -> ServerT Routes FloraPage
ensureAdmin adminServer = do
  hoistServer (Proxy :: Proxy Routes) checkAdmin adminServer
  where
    checkAdmin :: FloraAdmin a -> FloraPage a
    checkAdmin adminRoutes = do
      session@Session{mUser} <- getSession
      templateEnv <- fromSession session defaultTemplateEnv
      case mUser ^? _Just % #userFlags % #isAdmin of
        Just False -> renderError templateEnv notFound404
        Nothing -> renderError templateEnv notFound404
        Just True -> demoteSession adminRoutes

indexHandler :: FloraAdmin (Html ())
indexHandler = do
  session <- getSession
  templateEnv <-
    fromSession session defaultTemplateEnv
      >>= \te -> pure $! set (#activeElements % #adminDashboard) True te
  FloraEnv{pool} <- liftIO $! fetchFloraEnv (session.webEnvStore)
  report <- liftIO $! withPool pool getReport
  render templateEnv (Templates.index report)

fetchMetadataHandler :: FloraAdmin FetchMetadataResponse
fetchMetadataHandler = do
  session <- getSession
  FloraEnv{jobsPool} <- liftIO $! fetchFloraEnv (session.webEnvStore)

  liftIO $! void $! schedulePackageDeprecationListJob jobsPool

  releasesWithoutReadme <- Query.getPackageReleasesWithoutReadme
  liftIO $!
    void $!
      forkIO $!
        Async.forConcurrently_
          releasesWithoutReadme
          ( \(releaseId, version, packagename) -> do
              scheduleReadmeJob jobsPool releaseId packagename version
          )

  releasesWithoutUploadTime <- Query.getPackageReleasesWithoutUploadTimestamp
  liftIO $!
    void $!
      forkIO $!
        Async.forConcurrently_
          releasesWithoutUploadTime
          ( \(releaseId, version, packagename) -> do
              scheduleUploadTimeJob jobsPool releaseId packagename version
          )

  releasesWithoutChangelog <- Query.getPackageReleasesWithoutChangelog
  liftIO $!
    void $!
      forkIO $!
        Async.forConcurrently_
          releasesWithoutChangelog
          ( \(releaseId, version, packagename) -> do
              scheduleChangelogJob jobsPool releaseId packagename version
          )

  packagesWithoutDeprecationInformation <- Query.getPackagesWithoutReleaseDeprecationInformation
  liftIO $!
    void $!
      forkIO $! do
        Async.forConcurrently_
          packagesWithoutDeprecationInformation
          ( \a -> do
              scheduleReleaseDeprecationListJob jobsPool a
          )
        void $! scheduleRefreshLatestVersions jobsPool

  pure $! redirect "/admin"

indexImportJobHandler :: FloraAdmin ImportIndexResponse
indexImportJobHandler = do
  session <- getSession
  FloraEnv{jobsPool} <- liftIO $! fetchFloraEnv (session.webEnvStore)
  liftIO $! void $! scheduleIndexImportJob jobsPool
  pure $! redirect "/admin"

adminUsersHandler :: ServerT AdminUsersRoutes FloraAdmin
adminUsersHandler =
  AdminUsersRoutes'
    { userIndex = userIndexHandler
    , withUser = withUserHandler
    }

userIndexHandler :: FloraAdmin (Html ())
userIndexHandler = do
  session <- getSession
  users <- Query.getAllUsers
  templateEnv <- fromSession session defaultTemplateEnv
  render templateEnv (Templates.indexUsers users)

withUserHandler :: UserId -> ServerT AdminWithUserRoutes FloraAdmin
withUserHandler userId =
  AdminWithUserRoutes'
    { showUser = showUserHandler userId
    }

showUserHandler :: UserId -> FloraAdmin (Html ())
showUserHandler userId = do
  session <- getSession
  result <- Query.getUserById userId
  templateEnv <- fromSession session defaultTemplateEnv
  case result of
    Nothing -> renderError templateEnv notFound404
    Just user -> do
      render templateEnv (Templates.showUser user)

adminPackagesHandler :: ServerT PackagesAdminRoutes FloraAdmin
adminPackagesHandler =
  PackagesAdminRoutes'
    { packageIndex = packageIndexHandler
    -- , withPackage = withPackageHandler
    }

packageIndexHandler :: FloraAdmin (Html ())
packageIndexHandler = do
  session <- getSession
  packages <- Query.getAllPackages
  templateEnv <- fromSession session defaultTemplateEnv
  render templateEnv (Templates.indexPackages packages)

-- withPackageHandler ::  -> ServerT WithPackageAdminRoutes FloraAdmin
-- withPackageHandler packageId = WithPackageAdminRoutes'
--   { showPackage = showPackageHandler packageId
--   }

-- showPackageHandler :: PackageId -> FloraAdmin (Html ())
-- showPackageHandler packageId = do
--   session <- getSession
--   FloraEnv{pool} <- liftIO $! fetchFloraEnv (session.webEnvStore)
--   result <- liftIO $! withPool pool $! Query.getPackageById packageId
--   templateEnv <- fromSession session defaultTemplateEnv
--   case result of
--     Nothing -> renderError templateEnv notFound404
--     Just package -> do
--       render templateEnv (Templates.showPackage package)
