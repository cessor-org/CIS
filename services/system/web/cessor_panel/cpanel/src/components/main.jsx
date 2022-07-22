import React, { PureComponent } from "react";
import Home from "./main/home.jsx"
import Docs from "./main/docs.jsx"

class Main extends PureComponent {

    home=()=>{
        return (<Home
            theme = {this.props.theme}
            active = {this.props.active == "Home"}
            width = {this.props.width}
            onChange={this.fromHome}
        />);
    }
    fromHome=()=>{}
    docs =()=>{
        return (<Docs
            theme = {this.props.theme}
            active = {this.props.active == "Docs"}
            width = {this.props.width}
            height = {this.props.height}
            onChange={this.fromDocs}
        />);
    }
    fromDocs=()=>{}
    render() {
        let theme = this.props.theme ? "lightMode" : "darkMode";
        let className = `row mainComponent ${theme}`;
        //<div className="col-3" style={{backgroundColor:"red"}}></div>
        return (
            <div className="container">
                <div className={`row`} >
                    {this.home()}
                    {this.docs()}
                </div>
            </div>
        );
    }
}

export default Main;