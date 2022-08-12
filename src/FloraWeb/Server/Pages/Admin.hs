module FloraWeb.Server.Pages.Admin where

import Control.Monad.IO.Class
import Data.Proxy (Proxy (..))
import Database.PostgreSQL.Entity.DBT
import Effectful.Servant (handlerToEff)
import Lucid
import Network.HTTP.Types.Status (notFound404)
import qualified OddJobs.Endpoints as OddJobs
import qualified OddJobs.Types as OddJobs
import Optics.Core
import Servant (HasServer (..), hoistServer)

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Async as Async
import Control.Monad
import Flora.Environment (FloraEnv (..))
import Flora.Model.Admin.Report
import qualified Flora.Model.Package.Query as Query
import qualified Flora.Model.Release.Query as Query
import Flora.Model.User
import qualified Flora.Model.User.Query as Query
import Flora.OddJobs
import FloraWeb.Routes.Pages.Admin
import FloraWeb.Server.Auth
import FloraWeb.Server.Utils (redirect)
import FloraWeb.Session (getSession)
import FloraWeb.Templates (ActiveElements (..), TemplateEnv (..), defaultTemplateEnv, fromSession, render)
import qualified FloraWeb.Templates.Admin as Templates
import qualified FloraWeb.Templates.Admin.Packages as Templates
import qualified FloraWeb.Templates.Admin.Users as Templates
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
      , makeReadmes = makeReadmesHandler
      , fetchUploadTimes = fetchUploadTimesHandler
      , importIndex = indexImportJobHandler
      }

{- | This function converts a sub-tree of routes that require 'Admin' role
 to a sub-tree of Flora pages.
 It acts as the safeguard that rejects non-admins from protected routes.
-}
ensureAdmin :: ServerT Routes FloraAdmin -> ServerT Routes FloraPage
ensureAdmin adminServer = do
  hoistServer (Proxy :: Proxy Routes) checkAdmin adminServer
  where
    -- adminSession
    --   . runCurrentTimeIO
    --   . runDB
    --   . demoteSession
    --   $ m

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
      >>= \te -> pure $ set (#activeElements % #adminDashboard) True te
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  report <- liftIO $ withPool pool getReport
  render templateEnv (Templates.index report)

makeReadmesHandler :: FloraAdmin MakeReadmesResponse
makeReadmesHandler = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  releases <- Query.getPackageReleasesWithoutReadme
  liftIO $ forkIO $ forM_ releases $ \(releaseId, version, packagename) -> do
    scheduleReadmeJob pool releaseId packagename version
  pure $ redirect "/admin"

fetchUploadTimesHandler :: FloraAdmin FetchUploadTimesResponse
fetchUploadTimesHandler = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  releases <- Query.getPackageReleasesWithoutUploadTimestamp
  liftIO $ forkIO $ forM_ releases $ \(releaseId, version, packagename) -> do
    Async.async $ scheduleUploadTimeJob pool releaseId packagename version
  pure $ redirect "/admin"

indexImportJobHandler :: FloraAdmin FetchUploadTimesResponse
indexImportJobHandler = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  liftIO $ scheduleIndexImportJob pool
  pure $ redirect "/admin"

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
--   FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
--   result <- liftIO $ withPool pool $ Query.getPackageById packageId
--   templateEnv <- fromSession session defaultTemplateEnv
--   case result of
--     Nothing -> renderError templateEnv notFound404
--     Just package -> do
--       render templateEnv (Templates.showPackage package)
