module FloraWeb.Server.Logging.Metrics
  ( prometheusMiddleware
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Flora.Environment
import Network.Wai (Application, Request)
import qualified Network.Wai as Wai
import Network.Wai.Middleware.Prometheus as P

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
