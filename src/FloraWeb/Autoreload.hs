module FloraWeb.Autoreload where

-- import Control.Monad (void)
import Control.Monad.IO.Class
import FloraWeb.Server.Auth (FloraDevSocket)
import Network.WebSockets (Connection)
import qualified Network.WebSockets as Websocket
import Servant
import Servant.API.Generic
import Servant.API.WebSocket
import Data.Text (Text)
import qualified Data.Text.IO as T

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
        liftIO $ T.putStrLn msg
        handler conn
