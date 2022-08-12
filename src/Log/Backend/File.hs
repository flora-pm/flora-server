module Log.Backend.File where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Kind (Type)
import Effectful
import qualified Effectful.Log.Logger as Log
import GHC.Generics (Generic)
import Log (Logger)
import System.IO (stdout)

data FileBackendConfig = FileBackendConfig
  { destinationFile :: FilePath
  }
  deriving stock (Eq, Ord, Show, Generic)

withJSONFileBackend ::
  forall (es :: [Effect]) (a :: Type).
  IOE :> es =>
  FileBackendConfig ->
  (Logger -> Eff es a) ->
  Eff es a
withJSONFileBackend FileBackendConfig{destinationFile} action = do
  liftIO $ BS.hPutStrLn stdout $ BS.pack $ "Redirecting logs to " <> destinationFile
  logger <- Log.mkLogger "file-json" $ \msg -> liftIO $ do
    BS.appendFile destinationFile (BSL.toStrict $ Aeson.encode msg)
  Log.withLogger logger action
