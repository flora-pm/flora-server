module FloraWeb.Server.Logging.Metrics
  ( prometheusMiddleware
  ) where

import Network.Wai (Application, Request)
import Network.Wai.Middleware.Prometheus as P
import qualified Network.Wai as Wai
import Data.Text (Text)
import qualified Data.Text as T

prometheusMiddleware :: Application -> Application
prometheusMiddleware = P.prometheus config . instrument normalizeWaiRequestRoute
  where
    config = PrometheusSettings ["metrics"] False True
    instrument = P.instrumentHandlerValueWithFilter P.ignoreRawResponses

normalizeWaiRequestRoute ::Request -> Text
normalizeWaiRequestRoute req = pathInfo
  where
    pathInfo :: Text
    pathInfo = "/" <> T.intercalate "/" (Wai.pathInfo req)
