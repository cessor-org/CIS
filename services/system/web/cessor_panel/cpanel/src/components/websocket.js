const Http = new XMLHttpRequest();
export class WebSock {
  constructor(callback) {
    this.callback = callback;
    this.timeout = 250; // Initial timeout duration as a class variable
    let loc = window.location;
    //this.url = "ws://" + loc.host;
    this.url = "ws://localhost:1234";
    this.connectInterval = null;
    this.cn_state = false;
  }
  connect = () => {
    console.log("websocket connecting", this.url);
    this.callback({ msg: "connecting" });
    this.ws = new WebSocket(this.url);
    this.ws_events();
  };
  ws_events = () => {
    this.ws.onopen = () => {
      this.callback({ msg: "open" });
      console.log("connected websocket main component");
      this.timeout = 250; // reset timer to 250 on open of websocket connection
      clearTimeout(this.connectInterval); // clear Interval on on open of websocket connection
      this.cn_state = true;
    };
    this.ws.onmessage = (message) => {
        console.log("ws got msg", message.data);
        /*
      let msg = JSON.parse(message.data);
      console.log("ws got msg", message, msg);
      if (msg.cookie_update) {
        console.log("ws got cookie", msg);
        const url = window.location + "cookie_req";
        Http.open("GET", url);
        Http.send();
        Http.onreadystatechange = (e) => {
          console.log(Http.responseText);
        };
      } else this.callback({ msg: msg });
      */
    };
    this.ws.onclose = (e) => {
      this.callback({ close: true });
      console.log(
        `Socket is closed. Reconnect will be attempted in ${Math.min(
          10000 / 1000,
          (this.timeout + this.timeout) / 1000
        )} second.`,
        e.reason
      );
      this.timeout = this.timeout + this.timeout; //increment retry interval
      this.connectInterval = setTimeout(
        this.check,
        Math.min(10000, this.timeout)
      ); //call check function after timeout
      this.cn_state = false;
    };
    this.ws.onerror = (err) => {
      this.callback({ err: err });
      console.error(
        "Socket encountered error: ",
        err.message,
        "Closing socket"
      );
      this.ws.close();
      this.cn_state = false;
    };
  };
  send = (msg) => {
    //let json = JSON.stringify(msg);
    let json = msg;
    this.ws.send(json);
  };
  check = () => {
    if (!this.ws || this.ws.readyState === WebSocket.CLOSED) this.connect(); //check if websocket instance is closed, if so call `connect` function.
  };
}