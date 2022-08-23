module FloraWeb.Server.Tracing where

import Control.Exception (SomeException, throw)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 (unpack)
import Data.Text.Display (display)
import Flora.Environment
import Log (LogLevel (..), Logger, logAttention, runLogT)
import Network.Wai
import Network.Wai.Handler.Warp
import System.Log.Raven (initRaven, register, silentFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel (Error), SentryRecord (..))

onException :: Logger -> DeploymentEnv -> LoggingEnv -> Maybe Request -> SomeException -> IO ()
onException logger environment tracingEnv mRequest exception =
  case tracingEnv.sentryDSN of
    Nothing -> Log.runLogT "flora" logger LogAttention $ do
      logAttention "Unhandled exception" $
        Aeson.object ["exception" .= display (show exception)]
      throw exception
    Just sentryDSN -> do
      sentryService <-
        initRaven
          sentryDSN
          (\defaultRecord -> defaultRecord{srEnvironment = Just $ show environment})
          sendRecord
          silentFallback
      register
        sentryService
        "flora-logger"
        Error
        (formatMessage mRequest exception)
        (recordUpdate mRequest exception)
      defaultOnException mRequest exception

formatMessage :: Maybe Request -> SomeException -> String
formatMessage Nothing exception = "Exception before request could be parsed: " ++ show exception
formatMessage (Just request) exception = "Exception " ++ show exception ++ " while handling request " ++ show request

recordUpdate :: Maybe Request -> SomeException -> SentryRecord -> SentryRecord
recordUpdate Nothing _exception record = record
recordUpdate (Just request) _exception record =
  record
    { srCulprit = Just $ unpack $ rawPathInfo request
    , srServerName = unpack <$> requestHeaderHost request
    }
