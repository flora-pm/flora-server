module FloraWeb.LiveReload (livereloadHandler) where

import Control.Monad.IO.Class
import Network.WebSockets (Connection)
import qualified Data.Text.IO as Text
import qualified Network.WebSockets as Websocket

import Effectful
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson
import Data.Version (Version)
import qualified Data.Text as Text
import Debug.Trace

data LiveReloadCommand = 
  Hello HelloCommand
  deriving stock (Eq, Ord, Show, Generic)

data HelloCommand = HelloCommand 
  { protocols :: [Text]
  , version :: Version
  }
  deriving stock (Eq, Ord, Show, Generic)

instance FromJSON LiveReloadCommand where
  parseJSON = withObject "command" $ \o -> do
    commandName :: Text <- o .: "command"
    case commandName of
      "hello" -> do
        protocols <- o .: "protocols" 
        traceShowM protocols
        version <- o .: "ver"
        traceShowM version
        pure $ Hello HelloCommand{..}
      _ -> fail "meh"

livereloadHandler :: (IOE :> es) => Connection -> Eff es ()
livereloadHandler connection = do
  handler connection
  where
    handler conn = do
      msg <- liftIO (Websocket.receiveData connection)
      liftIO $ Text.putStrLn msg
      case eitherDecodeStrictText msg of
        Right jsonData -> do
          liftIO $ Text.putStrLn jsonData
          handler conn
        Left errors -> do
          liftIO $ Text.putStrLn $ Text.pack errors
