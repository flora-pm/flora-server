module FloraWeb.Server.Logging.Metrics
  ( prometheusMiddleware
  ) where

import Network.Wai (Application, Request)
import Network.Wai.Middleware.Prometheus as P
import qualified Network.Wai as Wai
import Data.Text (Text)
import qualified Data.Text as T
import Flora.Environment

prometheusMiddleware :: LoggingEnv -> Application -> Application
prometheusMiddleware LoggingEnv{prometheusEnabled} =
  if prometheusEnabled
  then P.prometheus config . instrument normalizeWaiRequestRoute
  else id
  where
    config = PrometheusSettings ["metrics"] False True
    instrument = P.instrumentHandlerValueWithFilter P.ignoreRawResponses

normalizeWaiRequestRoute ::Request -> Text
normalizeWaiRequestRoute req = pathInfo
  where
    pathInfo :: Text
    pathInfo = "/" <> T.intercalate "/" (Wai.pathInfo req)
