module FloraWeb.Server where

import Colourista.IO (blueMessage)
import Control.Exception (bracket)

import Control.Exception.Safe qualified as Safe
import Control.Monad (void, when)
import Data.Aeson qualified as Aeson
import Data.Maybe (isJust)
import Data.OpenApi (OpenApi)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text.Display (display)
import Database.PostgreSQL.Simple (Connection)
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Static
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Fail (runFailIO)
import Effectful.PostgreSQL.Transact.Effect (runDB)
import Effectful.Reader.Static (runReader, withReader)
import Effectful.Time (runTime)
import Log (Logger)
import Log qualified
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (notFound404)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setOnException
  , setPort
  )
import Network.Wai.Log qualified as WaiLog
import Network.Wai.Middleware.Heartbeat (heartbeatMiddleware)
import OddJobs.Endpoints qualified as OddJobs
import OddJobs.Job (startJobRunner)
import OddJobs.Types qualified as OddJobs
import Optics.Core
import Prometheus qualified
import Prometheus.Metric.GHC (ghcMetrics)
import Prometheus.Metric.Proc (procMetrics)
import Servant
  ( Application
  , Context (..)
  , ErrorFormatters
  , Handler
  , HasServer (hoistServerWithContext)
  , NotFoundErrorFormatter
  , Proxy (Proxy)
  , defaultErrorFormatters
  , err404
  , hoistServer
  , notFoundErrorFormatter
  , serveDirectoryWebApp
  , serveDirectoryWith
  )
import Servant.API (getResponse)
import Servant.OpenApi
import Servant.Server.Generic (AsServerT, genericServeTWithContext)

import Flora.Environment (DeploymentEnv, FloraEnv (..), LoggingEnv (..), getFloraEnv)
import Flora.Environment.Config (Assets)
import Flora.Logging (runLog)
import Flora.Logging qualified as Logging
import FloraJobs.Runner (runner)
import FloraJobs.Types (JobsRunnerEnv (..), makeConfig, makeUIConfig)
import FloraWeb.API.Routes qualified as API
import FloraWeb.API.Server qualified as API
import FloraWeb.Common.Auth (FloraAuthContext, authHandler, requestID, runVisitorSession)
import FloraWeb.Common.Metrics
import FloraWeb.Common.OpenSearch
import FloraWeb.Common.Tracing
import FloraWeb.Common.Utils
import FloraWeb.Embedded
import FloraWeb.Pages.Routes qualified as Pages
import FloraWeb.Pages.Server qualified as Pages
import FloraWeb.Pages.Templates (defaultTemplateEnv, defaultsToEnv)
import FloraWeb.Pages.Templates.Error (renderError)
import FloraWeb.Routes
import FloraWeb.Types

runFlora :: IO ()
runFlora =
  bracket
    (getFloraEnv & runFailIO & runEff)
    (runEff . shutdownFlora)
    ( \env ->
        runEff . runTime . runConcurrent $ do
          let baseURL = "http://localhost:" <> display (env.httpPort)
          liftIO $ blueMessage $ "🌺 Starting Flora server on " <> baseURL
          liftIO $ when (isJust $ env.logging.sentryDSN) (blueMessage "📋 Connected to Sentry endpoint")
          liftIO $ when env.logging.prometheusEnabled $ do
            blueMessage $ "📋 Service Prometheus metrics on " <> baseURL <> "/metrics"
            void $ Prometheus.register ghcMetrics
            void $ Prometheus.register procMetrics
          let withLogger = Logging.makeLogger (env.logging.logger)
          withLogger
            ( \appLogger ->
                runServer appLogger env
            )
    )

shutdownFlora :: FloraEnv -> Eff '[IOE] ()
shutdownFlora env =
  liftIO $
    Pool.destroyAllResources (env.pool)

logException
  :: DeploymentEnv
  -> Logger
  -> Safe.SomeException
  -> IO ()
logException env logger exception =
  runEff
    . runTime
    . Logging.runLog env logger
    $ Log.logAttention "odd-jobs runner crashed " (show exception)

runServer :: (Concurrent :> es, IOE :> es) => Logger -> FloraEnv -> Eff es ()
runServer appLogger floraEnv = do
  httpManager <- liftIO $ HTTP.newManager tlsManagerSettings
  let runnerEnv = JobsRunnerEnv httpManager floraEnv.githubToken
  let oddjobsUiCfg = makeUIConfig (floraEnv.config) appLogger (floraEnv.jobsPool)
      oddJobsCfg =
        makeConfig
          runnerEnv
          (floraEnv.config)
          appLogger
          (floraEnv.jobsPool)
          runner

  void $ forkIO $ unsafeEff_ $ Safe.withException (startJobRunner oddJobsCfg) (logException (floraEnv.environment) appLogger)
  loggingMiddleware <- Logging.runLog (floraEnv.environment) appLogger WaiLog.mkLogMiddleware
  oddJobsEnv <- OddJobs.mkEnv oddjobsUiCfg ("/admin/odd-jobs/" <>)
  let webEnv = WebEnv floraEnv
  webEnvStore <- liftIO $ newWebEnvStore webEnv
  let server = mkServer appLogger webEnvStore floraEnv oddjobsUiCfg oddJobsEnv
  let warpSettings =
        setPort (fromIntegral $ floraEnv.httpPort) $
          setOnException
            ( onException
                appLogger
                (floraEnv.environment)
                (floraEnv.logging)
            )
            defaultSettings
  liftIO
    $ runSettings warpSettings
    $ prometheusMiddleware (floraEnv.environment) (floraEnv.logging)
      . heartbeatMiddleware
      . loggingMiddleware
      . const
    $ server

mkServer
  :: Logger
  -> WebEnvStore
  -> FloraEnv
  -> OddJobs.UIConfig
  -> OddJobs.Env
  -> Application
mkServer logger webEnvStore floraEnv cfg jobsRunnerEnv = do
  genericServeTWithContext
    (naturalTransform (floraEnv.environment) logger webEnvStore)
    (floraServer (floraEnv.pool) cfg jobsRunnerEnv)
    (genAuthServerContext logger floraEnv)

-- What the fuck is happening here:
--
-- In 'pages' and 'api', we have to reconcile two list of effects:
--  pages has effects:
--    [IsVisitor, DB, Time, Reader (Headers '[Header "Set-Cookie" SetCookie] Session), Log, Error ServerError, IOE]
--  api has effects:
--    [DB, Time, Reader (), Log, Error ServerError, IOE]
--  An the intermediate effect list of effects:
--    [Reader WebEnvStore, Log, Error ServerError, IOE]
--
-- What must happen is that the list of effects of 'pages' and 'api' must correspond to the intermediate 'Flora'
-- list of effects. For 'pages', we can change the 'Reader Session' to a 'Reader WebEnvStore',
-- but for 'api' there is no such Reader to transform in the first place, so we put an artificial
-- Reader that we can transform to make the types match.
floraServer
  :: Pool Connection
  -> OddJobs.UIConfig
  -> OddJobs.Env
  -> Routes (AsServerT Flora)
floraServer pool cfg jobsRunnerEnv =
  Routes
    { assets = serveDirectoryWebApp "./static"
    , openSearch = openSearchHandler
    , pages = \sessionWithCookies ->
        hoistServerWithContext
          (Proxy @Pages.Routes)
          (Proxy @'[FloraAuthContext])
          ( \floraPage ->
              floraPage
                & runVisitorSession
                & runDB pool
                & Log.localData [("request_id", Aeson.String $ requestID . getResponse $ sessionWithCookies)]
                & runTime
                & withReader (const sessionWithCookies)
          )
          (Pages.server cfg jobsRunnerEnv)
    , api =
        hoistServer
          (Proxy @API.Routes)
          ( \floraPage ->
              floraPage
                & runDB pool
                & runTime
                & withReader (const ())
          )
          API.apiServer
    , openApi = pure openApiHandler
    , docs = serveDirectoryWith docsBundler
    }

naturalTransform :: DeploymentEnv -> Logger -> WebEnvStore -> Flora a -> Handler a
naturalTransform deploymentEnv logger webEnvStore app =
  app
    & runReader webEnvStore
    & runLog deploymentEnv logger
    & effToHandler

genAuthServerContext :: Logger -> FloraEnv -> Context '[FloraAuthContext, ErrorFormatters]
genAuthServerContext logger floraEnv = authHandler logger floraEnv :. errorFormatters floraEnv.assets :. EmptyContext

errorFormatters :: Assets -> ErrorFormatters
errorFormatters assets =
  defaultErrorFormatters{notFoundErrorFormatter = notFoundPage assets}

notFoundPage :: Assets -> NotFoundErrorFormatter
notFoundPage assets _req =
  let result = runPureEff $ runErrorNoCallStack $ renderError (defaultsToEnv assets defaultTemplateEnv) notFound404
   in case result of
        Left err -> err
        Right _ -> err404

openApiHandler :: OpenApi
openApiHandler =
  toOpenApi (Proxy @API.Routes)
    & (#info % #title .~ "Flora API")
    & (#info % #version .~ "v0")
    & (#info % #description ?~ "Flora API Documentation")
