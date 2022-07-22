export class WS_ban {
    constructor(callback) {
    }
    watch=()=>{
        console.log("watch");
        var WebSocket2 = WebSocket;
        
        WebSocket = function(addy) {
            console.log('WS: Trying to open.', addy);
            var ws;
            if (!this.blocked) {
                console.log('WS: Not blocked, allowing.');
                ws = new WebSocket(addy);
                this.open_sockets.push(ws);
                return ws;
            } else {
                console.log('WS: Blocked.');
            }
        };
        
        WebSocket.toggle = function() {
            WebSocket.prototype.blocked = !WebSocket.prototype.blocked;
            var sockets = WebSocket.prototype.open_sockets;
            if (WebSocket.prototype.blocked) {
                console.log('WS: Blocking. Removing Old Sockets.');
                sockets.forEach(function(socket, index, sockets) {
                    console.log("WS: Closing -", index, socket);
                    socket.close();
                });
                WebSocket.prototype.open_sockets = [];
                console.log("WS: Sockets left open -", WebSocket.prototype.open_sockets.length);
            } else {
                console.log("WS: Unblocking");
            }
        };

        WebSocket.prototype.open_sockets = [];
        WebSocket.prototype.blocked =true;
    }
}