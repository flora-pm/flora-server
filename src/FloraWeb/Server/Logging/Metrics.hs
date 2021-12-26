module FloraWeb.Server.Logging.Metrics
  ( prometheusMiddleware
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display
import Data.Text.Encoding
import Flora.Environment
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Application, Request)
import qualified Network.Wai as Wai
import Network.Wai.Middleware.Prometheus (PrometheusSettings (..))
import qualified Network.Wai.Middleware.Prometheus as P
import Prometheus as P
import System.Clock (Clock (..), getTime)

prometheusMiddleware :: DeploymentEnv -> LoggingEnv -> Application -> Application
prometheusMiddleware environment LoggingEnv{prometheusEnabled} =
  if prometheusEnabled
  then P.prometheus config . instrument
  else id
  where
    config = PrometheusSettings ["metrics"] False True
    instrument :: Application -> Application
    instrument =
      instrumentHandlerValueWithFilter (show environment) P.ignoreRawResponses normalizeWaiRequestRoute

normalizeWaiRequestRoute ::Request -> Text
normalizeWaiRequestRoute req = pathInfo
  where
    pathInfo :: Text
    pathInfo = "/" <> T.intercalate "/" (Wai.pathInfo req)

countRoute :: Text
           -> Text       -- ^ handler label
           -> Text -- ^ method
           -> Text -- ^ status
           -> IO ()
countRoute environment handler method status =
  P.withLabel routeCounter (handler, method, status, environment) P.incCounter

routeCounter :: P.Vector P.Label4 P.Counter
routeCounter = P.unsafeRegister $ P.vector ("handler", "method", "status_code", "environment")
                                $ P.counter info
  where
    info = P.Info "route_counter" "How many times was this route accessed"
{-# NOINLINE routeCounter #-}

instrumentHandlerValueWithFilter ::
  String
  -> (Wai.Response -> Maybe Wai.Response) -- ^ Response filter
  -> (Wai.Request -> Text) -- ^ The function used to derive the "handler" value in Prometheus
  -> Wai.Application -- ^ The app to instrument
  -> Wai.Application -- ^ The instrumented app
instrumentHandlerValueWithFilter environment resFilter f app req respond = do
  start <- getTime Monotonic
  app req $ \res -> do
    case resFilter res of
      Nothing -> return ()
      Just res' -> do
        end <- getTime Monotonic
        let method = decodeUtf8 (Wai.requestMethod req)
        let status = T.pack (show (HTTP.statusCode (Wai.responseStatus res')))
        countRoute (f req) method status (display environment)
        P.observeSeconds (f req) (Just method) (Just status) start end
    respond res
