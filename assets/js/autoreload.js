const initialReconnectDelay = 1000;
const maxReconnectDelay = 16000;
var currentReconnectDelay = initialReconnectDelay;

const connectToServer = () => {
  sessionStorage.setItem("connId", null);
  console.log("[+] Connecting to the autoreload websocket…")
  const autoreloadSocket = new WebSocket("ws://localhost:8083/sockets/autoreload");
  autoreloadSocket.addEventListener('close',  onWebsocketClose);
  autoreloadSocket.addEventListener('error', onWebsocketClose);
  autoreloadSocket.addEventListener('open',  (event) => onWebsocketOpen(autoreloadSocket, event));
  autoreloadSocket.addEventListener('message',  onWebsocketMessage);
}

const onWebsocketOpen = (socket, event) => {
  console.log("[+] Connection to server established!");
  socket.send("HELLO");
}

const onWebsocketMessage = (event) => {
  console.log({event});
  console.log(event.data);
  console.log(typeof sessionStorage.getItem("connId"));
  console.log(`[+] connection Id from the server is: ${event.data}`);
  console.log(`[+] connection Id in the session is: ${sessionStorage.getItem("connId")}`);

  const sessionConnId = sessionStorage.getItem("connId");
  if (sessionConnId === null || sessionConnId === "null" ) {
    console.log("[+] session connId is the following type:");
    console.log(typeof sessionStorage.getItem("connId"));
    console.log("[+] Connection id is null");
    console.log("[+] Saving the server connId into the sessionStorage")
    sessionStorage.setItem("connId", event.data);
  } else {
    console.log("[+] session connId is the following type:");
    console.log(typeof sessionStorage.getItem("connId"));
  }

  if (sessionStorage.getItem("connId") !== event.data) {
    console.log("[+] session storage connId is different from the event data");
    console.log("[+] Reloading the page");
    location.reload(true);
  } else {
    console.log("[+] session storage connId and server connId are similar");
  }
}

const onWebsocketClose = () => {
  console.log("[!] Server disconnected, attempting to reconnect…");
  sessionStorage.setItem("connId", null);
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
