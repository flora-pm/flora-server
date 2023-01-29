module FloraWeb.Autoreload where

-- import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text.IO qualified as T
import FloraWeb.Server.Auth (FloraDevSocket)
import Network.WebSockets (Connection)
import Network.WebSockets qualified as Websocket
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
autoreloadHandler connection =
  handler connection
  where
    handler :: Connection -> FloraDevSocket ()
    handler conn = do
      msg <- liftIO (Websocket.receiveData connection :: IO Text)
      liftIO $! T.putStrLn msg
      handler conn
