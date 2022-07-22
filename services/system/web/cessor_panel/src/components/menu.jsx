import React, { PureComponent } from "react";
import {
    faNetworkWired,
    faUser,
    faCoins,
    faCog,
    faDiceD20,
    faFile,
    faInfoCircle,
    faThList
  } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";

class Menu extends PureComponent {
    constructor(props) {
        super(props);
        this.state = {
            bodyList : [
                {
                    title : "System",
                    icon: faDiceD20
                },
                {
                    title : "Network",
                    icon: faNetworkWired
                },
                {
                    title : "Apps",
                    icon: faThList
                },
                {
                    title : "Wallet",
                    icon: faCoins
                },
                {
                    title : "Docs",
                    icon: faFile
                },
                {
                    title : "About",
                    icon: faInfoCircle
                }
            ],
            footerList : [
                {
                    title : "Settings",
                    icon: faCog
                },
                {
                    title : "Account",
                    icon: faUser
                }
            ]
        }
    }
    header = ()=>{
        let theme = this.props.theme ? "lightMode" : "darkMode";
        let active = this.props.active === "Home" ? "active" : "";
        let className = `col-12 menu header ${theme} ${active}`
        return (
            <div
                key={`menu-header`}
                className={`row`}
                style={{margin: 0, padding:0}}
                onClick={()=>this.activate("Home")}>
                <div className={className}>
                    {"C§"}
                </div>
            </div>
        );
    }
    body = ()=>{
        let theme = this.props.theme ? "lightMode" : "darkMode";
        let className = `col-12 menu body ${theme}`
        return this.state.bodyList.map((element, index)=>(
            <div 
                key={`menu-body-${index}`}
                className={`row`}
                style={{margin: 0, padding:0}}
                onClick={()=>this.activate(element.title)}>
                <div 
                    className={`${className} ${this.props.active === element.title ? "active" : ""}`}>
                    <FontAwesomeIcon icon={element.icon} className="icon"/>
                    <p className="title">{element.title}</p>
                </div>
            </div>
        ));
    }
    footer =()=>{
        let theme = this.props.theme ? "lightMode" : "darkMode";
        let className = `col-12 menu footer ${theme}`
        return this.state.footerList.map((element, index)=>(
            <div
                key={`menu-footer-${index}`} 
                className={`row`}
                style={{margin: 0, padding:0}}>
                <div
                    className={`${className} 
                        ${this.props.active === element.title ? "active" : ""}`}
                    style={{bottom:index*70}}
                    onClick={()=>this.activate(element.title)}>
                    <FontAwesomeIcon icon={element.icon} className="icon"/>
                    <p className="title">{element.title}</p>
                </div>
            </div>
        ));
    }

    activate=(element)=>{
        this.props.onChange({active: element});
    }
    
    render() {
        return (
            <div className="container" style={{margin: 0, padding:0}}>
                {this.header()}
                {this.body()}
                {this.footer()}
            </div>
        );
    }
}

export default Menu;