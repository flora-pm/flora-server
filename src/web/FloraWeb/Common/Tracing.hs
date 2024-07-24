module FloraWeb.Common.Tracing where

import Control.Exception (AsyncException (..), Exception (..), SomeException, throw)
import Control.Monad.IO.Class
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 (unpack)
import Data.Maybe (isJust)
import Data.Text.Display (display)
import GHC.IO.Exception (IOErrorType (..))
import Log (LogLevel (..), Logger, logAttention, runLogT)
import Network.Wai
import Network.Wai.Handler.Warp
import System.IO.Error (ioeGetErrorType)
import System.Log.Raven (initRaven, register, silentFallback)
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel (Error), SentryRecord (..))

import Flora.Environment

onException :: Logger -> DeploymentEnv -> MLTP -> Maybe Request -> SomeException -> IO ()
onException logger environment mltp mRequest exception =
  Log.runLogT "flora" logger LogAttention $ do
    case mltp.sentryDSN of
      Nothing -> do
        logAttention "Unhandled exception" $
          Aeson.object ["exception" .= display (show exception)]
        throw exception
      Just sentryDSN ->
        if shouldDisplayException exception && isJust mRequest
          then do
            sentryService <-
              liftIO $
                initRaven
                  sentryDSN
                  (\defaultRecord -> defaultRecord{srEnvironment = Just $ show environment})
                  sendRecord
                  silentFallback
            liftIO $
              register
                sentryService
                "flora-logger"
                Error
                (formatMessage mRequest exception)
                (recordUpdate mRequest exception)
            liftIO $ defaultOnException mRequest exception
          else liftIO $ defaultOnException mRequest exception

shouldDisplayException :: SomeException -> Bool
shouldDisplayException exception
  | Just ThreadKilled <- fromException exception = False
  | Just (_ :: InvalidRequest) <- fromException exception = False
  | Just (ioeGetErrorType -> et) <- fromException exception
  , et == ResourceVanished || et == InvalidArgument =
      False
  | Just ConnectionClosedByPeer <- fromException exception = False
  | otherwise = True

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
