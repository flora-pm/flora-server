

module FloraWeb.Common.Tracing where

import Control.Exception (AsyncException (..), Exception (..), SomeException, throw)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 (unpack)
import Data.Maybe (isJust)
import Data.Text.Display (display)
import Effectful
import Effectful.Exception qualified as E
import Effectful.Log
import GHC.IO.Exception (IOErrorType (..))
import Log qualified
import Network.Wai
import System.TimeManager
import Network.Wai.Handler.Warp
import System.IO.Error (ioeGetErrorType)
import System.Log.Raven
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel (..), SentryRecord (..))

import Flora.Environment.Config

onException
  :: Logger
  -> DeploymentEnv
  -> MLTP
  -> Maybe Request
  -> E.SomeException
  -> IO ()
onException logger environment mltp mRequest e@(E.SomeException exception) = do
  Log.runLogT "flora-production" logger LogAttention $ do
    let context = E.displayExceptionContext $ E.someExceptionContext e
    Log.logAttention "Unhandled exception" $
      Aeson.object
        [ "exception" .= display (show exception)
        , "backtraces" .= context
        ]
    case mltp.sentryDSN of
      Nothing -> throw exception
      Just sentryDSN ->
        if shouldDisplayException e && isJust mRequest
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
                "flora-server"
                Error
                (formatMessage mRequest e)
                (recordUpdate mRequest e)
            liftIO $ defaultOnException mRequest e
          else liftIO $ defaultOnException mRequest e

shouldDisplayException :: SomeException -> Bool
shouldDisplayException exception
  | Just ThreadKilled <- fromException exception = False
  | Just TimeoutThread <- fromException exception = False
  | Just ConnectionClosedByPeer <- fromException exception = False
  | Just (_ :: InvalidRequest) <- fromException exception = False
  | Just (ioeGetErrorType -> et) <- fromException exception
  , et == ResourceVanished || et == InvalidArgument =
      False
  | otherwise = True

formatMessage :: Maybe Request -> SomeException -> String
formatMessage Nothing exception = "Exception before request could be parsed: " ++ show exception
formatMessage (Just request) exception = "Exception " ++ show exception ++ " while handling request " ++ show request

recordUpdate :: Maybe Request -> SomeException -> SentryRecord -> SentryRecord
recordUpdate Nothing _exception rec = rec
recordUpdate (Just request) _exception rec =
  rec
    { srCulprit = Just $ unpack $ rawPathInfo request
    , srServerName = unpack <$> requestHeaderHost request
    }
