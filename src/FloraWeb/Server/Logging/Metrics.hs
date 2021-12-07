module FloraWeb.Server.Logging.Metrics
  ( prometheusMiddleware
  ) where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Flora.Environment
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Application, Request)
import qualified Network.Wai as Wai
import Network.Wai.Middleware.Prometheus (PrometheusSettings (..))
import qualified Network.Wai.Middleware.Prometheus as P
import Prometheus as P
import System.Clock (Clock (..), getTime)

prometheusMiddleware :: LoggingEnv -> Application -> Application
prometheusMiddleware LoggingEnv{prometheusEnabled} =
  if prometheusEnabled
  then P.prometheus config . instrument
  else id
  where
    config = PrometheusSettings ["metrics"] False True
    instrument :: Application -> Application
    instrument =
      instrumentHandlerValueWithFilter P.ignoreRawResponses normalizeWaiRequestRoute

normalizeWaiRequestRoute ::Request -> Text
normalizeWaiRequestRoute req = pathInfo
  where
    pathInfo :: Text
    pathInfo = "/" <> T.intercalate "/" (Wai.pathInfo req)

countRoute :: Text       -- ^ handler label
           -> Maybe Text -- ^ method
           -> Maybe Text -- ^ status
           -> IO ()
countRoute handler mmethod mstatus =
  P.withLabel routeCounter (handler, fromMaybe "" mmethod, fromMaybe "" mstatus) P.incCounter

routeCounter :: P.Vector P.Label3 P.Counter
routeCounter = P.unsafeRegister $ P.vector ("handler", "method", "status_code")
                                $ P.counter info
  where
    info = P.Info "route_counter" "How many times was this route accessed"
{-# NOINLINE routeCounter #-}

instrumentHandlerValueWithFilter ::
     (Wai.Response -> Maybe Wai.Response) -- ^ Response filter
  -> (Wai.Request -> Text) -- ^ The function used to derive the "handler" value in Prometheus
  -> Wai.Application -- ^ The app to instrument
  -> Wai.Application -- ^ The instrumented app
instrumentHandlerValueWithFilter resFilter f app req respond = do
  start <- getTime Monotonic
  app req $ \res -> do
    case resFilter res of
      Nothing -> return ()
      Just res' -> do
        end <- getTime Monotonic
        let method = Just $ decodeUtf8 (Wai.requestMethod req)
        let status = Just $ T.pack (show (HTTP.statusCode (Wai.responseStatus res')))
        countRoute (f req) method status
        P.observeSeconds (f req) method status start end
    respond res
