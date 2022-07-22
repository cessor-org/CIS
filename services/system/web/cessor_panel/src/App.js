import React, { Component } from 'react';
//  components
import Menu from "./components/menu.jsx"
import Main from "./components/main.jsx"
//import { WebSock } from "./components/websocket.js";
import { CessorDriver } from "./components/cessorDriver.js";

class App extends Component {

    constructor(props) {
        super(props);
        this.state = {
            theme: false,
            menuWidth : 60,
            footerHeight : 25,
            width : window.innerWidth,
            height : window.innerHeight,
            menu : {
                active : "Docs"
            },
            test: null
        }
        this.updateWindowDimensions = this.updateWindowDimensions.bind(this);
    }

    componentDidMount() {
        
        document.body.cessor = new CessorDriver((e) => this.cessorDriver(e));
        window.addEventListener('resize', this.updateWindowDimensions);
    }
    componentWillUnmount() {
        window.removeEventListener('resize', this.updateWindowDimensions);
    }
    cessorDriver = (event) => {
        //if(event.testKey && event.testKey == "testValue2")
            //this.update({theme:true});
    }
    menu = () => {
        let width = this.state.menuWidth;
        let height = this.state.height - this.state.footerHeight;
        let style = {width, height};
        let theme = this.state.theme ? "lightMode" : "darkMode";
        return (
            <div
                className = {`menuContainer ${theme}`}
                style={style}>
                <Menu
                    theme = {this.state.theme}
                    active = {this.state.menu.active}
                    onChange={this.fromMenu}
                />
            </div>
        )
    }
    fromMenu= (e)=>{
        if (e.active){
            this.update({menu: {active: e.active}});
        }
            
    }
    main = () => {
        let height = this.state.height - this.state.footerHeight;
        let width = this.state.width- this.state.menuWidth;
        let marginLeft = this.state.menuWidth;
        let style = {width, height, marginLeft};
        return (
            <div
                className="mainContainer"
                style={style}>
                <Main
                    theme = {this.state.theme}
                    width = {width}
                    height = {height}
                    active = {this.state.menu.active}
                    onChange={this.fromMain}
                />
            </div>
        )
        
    }
    fromMain= (e)=>{
    }
    footer = () => {
        let height = this.state.footerHeight;
        let marginTop = this.state.height - height;
        let style = {height, marginTop};
        return (
            <div
                className="footerContainer"
                style={style}>
                {""}
            </div>
        )
    }
    fromFooter= (e)=>{
    }
    updateWindowDimensions =() =>{
        let width= window.innerWidth;
        let height= window.innerHeight;
        console.log("width d: ", width);
        this.update({width, height});
      }
    update = (new_state) => {
        this.setState(new_state, function () {
          this.forceUpdate();
        });
    };

    render() {
        let className = `appContainer`;
        return (
        <div 
            className = {className}>
            {this.menu()}
            {this.main()}
            {this.footer()}
        </div>
        );
        //{this.cessorDriver_test()}
    }
}

export default App;