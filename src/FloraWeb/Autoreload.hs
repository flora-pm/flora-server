module FloraWeb.Autoreload where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import qualified Data.UUID as UUID
import FloraWeb.Server.Auth (FloraDevSocket)
import qualified Log
import Network.WebSockets (Connection)
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
  liftIO $ Websockets.receiveDataMessage connection
  liftIO $ Websockets.sendTextData connection (UUID.toText uuid)
