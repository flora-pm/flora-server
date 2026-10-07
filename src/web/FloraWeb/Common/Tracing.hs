module FloraWeb.Common.Tracing where

import Colourista.IO (blueMessage, redMessage)
import Control.Exception (AsyncException (..), Exception (..), IOException, SomeException, throw, try)
import Control.Monad (forM_, when)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 (unpack)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Effectful
import Effectful.Exception qualified as E
import Effectful.Log
import GHC.Eventlog.Socket qualified as Socket
import GHC.IO.Exception (IOErrorType (..))
import Log qualified
import Network.Wai
import Network.Wai.Handler.Warp
import System.Directory (createDirectoryIfMissing)
import System.Environment (getProgName)
import System.FilePath ((</>))
import System.IO.Error (ioeGetErrorType, ioeGetLocation)
import System.Log.Raven
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types (SentryLevel (..), SentryRecord (..))
import System.TimeManager (TimeoutThread (..))

import Flora.Environment.Config

handleExceptions
  :: Text
  -> Logger
  -> DeploymentEnv
  -> MLTP
  -> Maybe Request
  -> E.SomeException
  -> IO ()
handleExceptions componentName logger environment mltp mRequest e@(E.SomeException exception) = do
  Log.runLogT (componentName <> "-" <> display environment) logger LogAttention $ do
    let context = E.displayExceptionContext $ E.someExceptionContext e
    when (shouldDisplayException e) $ do
      Log.logAttention "Unhandled exception" $
        Aeson.object
          [ "exception" .= display (show exception)
          , "backtraces" .= context
          ]
      case mltp.sentryDSN of
        Nothing -> throw exception
        Just sentryDSN ->
          if isJust mRequest
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
  | Just (_ :: InvalidRequest) <- fromException exception = False
  | Just (ioeGetErrorType -> et) <- fromException exception
  , et == ResourceVanished || et == InvalidArgument =
      False
  | Just ioe <- fromException exception
  , ioeGetErrorType ioe == NoSuchThing
  , "kevent" `isInfixOf` ioeGetLocation ioe =
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

startEventlogSocket :: Maybe FilePath -> IO ()
startEventlogSocket mDirectory =
  forM_ mDirectory $ \directory -> do
    result <- try @IOException $ do
      createDirectoryIfMissing True directory
      progName <- getProgName
      Socket.start (directory </> progName <> ".sock")
    case result of
      Left err ->
        redMessage $
          "⚠️ Could not start the eventlog socket in "
            <> Text.pack directory
            <> ": "
            <> Text.pack (displayException err)
      Right () -> blueMessage "🔥 Sending live events to socket"
