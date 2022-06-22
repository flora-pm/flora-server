module FloraWeb.Server.Pages.Admin where

import Control.Monad.Reader
import Database.PostgreSQL.Entity.DBT
import Lucid
import Network.HTTP.Types.Status (notFound404)
import Optics.Core
import Servant

import Flora.Environment
import Flora.Model.Admin.Report
import qualified Flora.Model.Package.Query as Query
import Flora.Model.Release.Query
import Flora.Model.User
import qualified Flora.Model.User.Query as Query
import Flora.OddJobs
import FloraWeb.Routes.Pages.Admin
import FloraWeb.Server.Auth
import FloraWeb.Session (getSession)
import FloraWeb.Templates
import qualified FloraWeb.Templates.Admin as Templates
import qualified FloraWeb.Templates.Admin.Packages as Templates
import qualified FloraWeb.Templates.Admin.Users as Templates
import FloraWeb.Templates.Error
import FloraWeb.Types (fetchFloraEnv)
import Log.Class
import qualified OddJobs.Endpoints as OddJobs
import qualified OddJobs.Types as OddJobs

server :: OddJobs.UIConfig -> OddJobs.Env -> ServerT Routes FloraPageM
server cfg env =
  ensureAdmin $
    Routes'
      { index = indexHandler
      , users = adminUsersHandler
      , packages = adminPackagesHandler
      , oddJobs = OddJobs.server cfg env (lift . lift)
      , makeReadmes = makeReadmesHandler
      }

{- | This function converts a sub-tree of routes that require 'Admin' role
 to a sub-tree of Flora pages.
 It acts as the safeguard that rejects non-admins from protected routes.
-}
ensureAdmin :: ServerT Routes FloraAdminM -> ServerT Routes FloraPageM
ensureAdmin adminM = do
  hoistServer (Proxy :: Proxy Routes) checkAdmin adminM
  where
    checkAdmin :: FloraAdminM a -> FloraPageM a
    checkAdmin adminRoutes = do
      (Headers session@Session{sessionId, mUser} headers) <- ask
      templateEnv <- fromSession session defaultTemplateEnv
      case mUser ^? _Just % #userFlags % #isAdmin of
        Just True ->
          withReaderT
            ( \sessionWithCookies ->
                let Session{webEnvStore} = getResponse sessionWithCookies
                 in Headers (Session{..} :: Session 'Admin) headers
            )
            adminRoutes
        Just False -> renderError templateEnv notFound404
        Nothing -> renderError templateEnv notFound404

indexHandler :: FloraAdminM (Html ())
indexHandler = do
  session <- getSession
  templateEnv <- fromSession session defaultTemplateEnv >>= \te -> pure $ set (#activeElements % #adminDashboard) True te
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  report <- liftIO $ withPool pool getReport
  render templateEnv (Templates.index report)

makeReadmesHandler :: FloraAdminM (Html ())
makeReadmesHandler = localDomain "makeReadmesHandler" $ do
  logInfo "opening the readmes magic" ("" :: String)
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)

  logInfo "scheduling readme jabbb" ("" :: String)
  releases <- liftIO $ withPool pool getPackageReleases
  logInfo "got releases! qeueing the queue" ("" :: String)
  forM_ releases $ \(releaseId, version, packagename) -> do
    liftIO $ scheduleReadmeJob pool releaseId packagename version

  logInfo "done" ("exceptional" :: String)
  throwError $ err301{errHeaders = [("Location", "/admin")]}

adminUsersHandler :: ServerT AdminUsersRoutes FloraAdminM
adminUsersHandler =
  AdminUsersRoutes'
    { userIndex = userIndexHandler
    , withUser = withUserHandler
    }

userIndexHandler :: FloraAdminM (Html ())
userIndexHandler = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  users <- liftIO $ withPool pool Query.getAllUsers
  templateEnv <- fromSession session defaultTemplateEnv
  render templateEnv (Templates.indexUsers users)

withUserHandler :: UserId -> ServerT AdminWithUserRoutes FloraAdminM
withUserHandler userId =
  AdminWithUserRoutes'
    { showUser = showUserHandler userId
    }

showUserHandler :: UserId -> FloraAdminM (Html ())
showUserHandler userId = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  result <- liftIO $ withPool pool $ Query.getUserById userId
  templateEnv <- fromSession session defaultTemplateEnv
  case result of
    Nothing -> renderError templateEnv notFound404
    Just user -> do
      render templateEnv (Templates.showUser user)

adminPackagesHandler :: ServerT PackagesAdminRoutes FloraAdminM
adminPackagesHandler =
  PackagesAdminRoutes'
    { packageIndex = packageIndexHandler
    -- , withPackage = withPackageHandler
    }

packageIndexHandler :: FloraAdminM (Html ())
packageIndexHandler = do
  session <- getSession
  FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
  packages <- liftIO $ withPool pool Query.getAllPackages
  templateEnv <- fromSession session defaultTemplateEnv
  render templateEnv (Templates.indexPackages packages)

-- withPackageHandler ::  -> ServerT WithPackageAdminRoutes FloraAdminM
-- withPackageHandler packageId = WithPackageAdminRoutes'
--   { showPackage = showPackageHandler packageId
--   }

-- showPackageHandler :: PackageId -> FloraAdminM (Html ())
-- showPackageHandler packageId = do
--   session <- getSession
--   FloraEnv{pool} <- liftIO $ fetchFloraEnv (session ^. #webEnvStore)
--   result <- liftIO $ withPool pool $ Query.getPackageById packageId
--   templateEnv <- fromSession session defaultTemplateEnv
--   case result of
--     Nothing -> renderError templateEnv notFound404
--     Just package -> do
--       render templateEnv (Templates.showPackage package)
