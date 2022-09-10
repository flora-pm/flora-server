module Log.Backend.File where

import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Kind (Type)
import Effectful
import Effectful.Log.Logger qualified as Log
import GHC.Generics (Generic)
import Log (Logger)
import System.IO (stdout)

data FileBackendConfig = FileBackendConfig
  { destinationFile :: FilePath
  }
  deriving stock (Eq, Ord, Show, Generic)

withJSONFileBackend
  :: forall (es :: [Effect]) (a :: Type)
   . IOE :> es
  => FileBackendConfig
  -> (Logger -> Eff es a)
  -> Eff es a
withJSONFileBackend FileBackendConfig{destinationFile} action = do
  liftIO $ BS.hPutStrLn stdout $ BS.pack $ "Redirecting logs to " <> destinationFile
  logger <- Log.mkLogger "file-json" $ \msg -> liftIO $ do
    BS.appendFile destinationFile (BSL.toStrict $ Aeson.encode msg <> "\n")
  Log.withLogger logger action
