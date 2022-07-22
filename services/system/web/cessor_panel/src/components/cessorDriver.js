export class CessorDriver {
    constructor(callback) {
      this.callback = callback;
      this.reqList = [];
      this.version = "app";
    }
    /**
     * @param {any} response
     */
    get getReq() {
      return this.getRequest();
    }
    get getVer() {
        return this.version;
      }
    /**
     * @param {any} request
     */
    set setReq(request) {
        this.setRequest(request);
    }
    /**
     * @param {any} response
     */
    set setRes(response) {
        this.callback(response);
    }
    
    getRequest() {
        if (this.reqList.length > 0) {
            let request = this.reqList[0];
            this.reqList.shift();
            return request;
        } else return false;
    }
    setRequest(request){
        this.reqList.push(request);
        this.notification();
        
    }
    notification(){
        let XML_http_request = new XMLHttpRequest();
        let url = "LOADREQUEST";
        XML_http_request.open("GET", url);
        XML_http_request.send();
    }
  }

  function init ()
  {
      if (!document.body.cessor) {
          document.body.cessor = new CessorDriver();
      }
      return true;
  }
  init();