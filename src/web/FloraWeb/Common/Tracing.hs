{-# LANGUAGE RecordWildCards #-}

module FloraWeb.Common.Tracing where

import Control.Exception (AsyncException (..), Exception (..), SomeException, throw)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 (unpack)
import Data.Text.Display (display)
import Effectful
import Effectful.Exception qualified as E
import Effectful.Log
import GHC.IO.Exception (IOErrorType (..))
import GHC.Stack
import Log (logAttention)
import Network.Wai
import Network.Wai.Handler.Warp
import RequireCallStack
import System.IO.Error (ioeGetErrorType)
import System.Log.Raven.Types (SentryRecord (..))

onException
  :: (Log :> es, RequireCallStack)
  => [E.Handler (Eff es) a]
onException =
  [ E.Handler $ \(E.SomeException exception) -> do
      Log.logAttention "Unhandled exception" $
        Aeson.object
          [ "exception" .= display (show exception)
          , "backtraces" .= map formatFunCall (getCallStack callStack)
          ]
      throw exception
  ]
  where
    formatFunCall :: (String, SrcLoc) -> String
    formatFunCall (fun, SrcLoc{..}) =
      fun
        ++ " at "
        ++ srcLocFile
        ++ ":"
        ++ show srcLocStartLine
        ++ ":"
        ++ show srcLocStartCol

shouldDisplayException :: SomeException -> Bool
shouldDisplayException exception
  | Just ThreadKilled <- fromException exception = False
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
recordUpdate Nothing _exception record = record
recordUpdate (Just request) _exception record =
  record
    { srCulprit = Just $ unpack $ rawPathInfo request
    , srServerName = unpack <$> requestHeaderHost request
    }
