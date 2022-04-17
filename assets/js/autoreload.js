const initialReconnectDelay = 1000;
const maxReconnectDelay = 16000;
var currentReconnectDelay = initialReconnectDelay;

const connectToServer = () => {
  console.log("[+] Connecting to the autoreload websocket")
  const autoreloadSocket = new WebSocket("ws://localhost:8083/sockets/autoreload");
  autoreloadSocket.addEventListener('close',  onWebsocketClose);
  autoreloadSocket.addEventListener('error', onWebsocketClose);
  autoreloadSocket.addEventListener('open',  onWebsocketOpen);
  autoreloadSocket.addEventListener('message',  onWebsocketMessage);
}

const onWebsocketOpen = (event) => {
  console.log("[+] Connection to server established!");
}

const onWebsocketMessage = (event) => {
  console.log({event});
  if (window.connId === undefined || window.connId === null) {
    window.connId = event.data;
  }

  if (window.connId !== event.data) {
    console.log("[+] Reloading the page");
    location.reload(true);
  }
}

const onWebsocketClose = () => {
  console.log("[!] Server disconnected, attempting to reconnect…");
  window.connId = null;
  setTimeout(() => {
      reconnectToWebsocket();
  }, currentReconnectDelay);
}

const reconnectToWebsocket = () => {
  if(currentReconnectDelay < maxReconnectDelay) {
      currentReconnectDelay*=2;
  }
  connectToServer();
}

connectToServer();
