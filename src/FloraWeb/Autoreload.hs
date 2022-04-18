{-# LANGUAGE TemplateHaskell #-}

module FloraWeb.Autoreload where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import FloraWeb.Server.Auth (FloraDevSocket)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Log
import Network.WebSockets (Connection, DataMessage (..))
import qualified Network.WebSockets as Websockets
import Servant
import Servant.API.Generic
import Servant.API.WebSocket

type AutoreloadRoute = NamedRoutes AutoreloadRoute'
data AutoreloadRoute' mode = AutoreloadRoute'
  { autoreload :: mode :- "sockets" :> "autoreload" :> WebSocket
  }
  deriving stock (Generic)

server :: ServerT AutoreloadRoute FloraDevSocket
server =
  AutoreloadRoute'
    { autoreload = autoreloadHandler
    }

autoreloadHandler :: Connection -> FloraDevSocket ()
autoreloadHandler connection = do
  uuid <- ask
  Log.logInfo_ $ "Sending " <> UUID.toText uuid
  Log.logInfo_ "lol"
  liftIO $ Websockets.receiveDataMessage connection >>= printTextMessage
  liftIO $ Websockets.sendTextData connection (UUID.toText uuid)
  autoreloadHandler connection

printTextMessage :: (MonadIO m) => DataMessage -> m ()
printTextMessage (Text bs _) =
  liftIO $ T.putStrLn $ T.toStrict $ TL.decodeUtf8 bs
printTextMessage _ = pure ()

genUUID :: UUID
genUUID = $(runIO UUID.nextRandom >>= lift)
{-# NOINLINE genUUID #-}
