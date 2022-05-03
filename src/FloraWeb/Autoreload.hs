{-# LANGUAGE TemplateHaskell #-}

module FloraWeb.Autoreload where

import FloraWeb.Server.Auth (FloraDevSocket)
import Network.WebSockets (Connection)
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
  autoreloadHandler connection
