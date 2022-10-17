// const initialReconnectDelay = 1000;
// const maxReconnectDelay = 16000;
// var currentReconnectDelay = initialReconnectDelay;

// var wasConnectionLost = false;

// const connectToServer = () => {
//   sessionStorage.setItem("connId", null);
//   console.log("[+] Connecting to the autoreload websocket…");
//   const autoreloadSocket = new WebSocket(
//     "ws://localhost:8083/sockets/autoreload"
//   );
//   autoreloadSocket.addEventListener("close", onWebsocketClose);
//   autoreloadSocket.addEventListener("error", onWebsocketClose);
//   autoreloadSocket.addEventListener("open", (event) =>
//     onWebsocketOpen(autoreloadSocket, event)
//   );
// };

// const onWebsocketOpen = (socket, event) => {
//   console.log("[+] Connection to server established!");
//   if (wasConnectionLost) {
//     location.reload(true);
//   }
// };

// const onWebsocketClose = () => {
//   console.log("[!] Server disconnected, attempting to reconnect…");
//   wasConnectionLost = true;
//   setTimeout(() => {
//     reconnectToWebsocket();
//   }, currentReconnectDelay);
// };

// const reconnectToWebsocket = () => {
//   if (currentReconnectDelay < maxReconnectDelay) {
//     currentReconnectDelay *= 2;
//   }
//   connectToServer();
// };

// connectToServer();
