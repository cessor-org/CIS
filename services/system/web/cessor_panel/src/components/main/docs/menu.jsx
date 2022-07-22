import React, { PureComponent } from "react";
import MenuElement from "./menu/element.jsx"
import {
    faChevronLeft,
    faChevronRight
  } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
class Menu extends PureComponent {
    constructor(props) {
        super(props);
        this.state = {}
    }
    componentDidMount(){}
    menu=()=>{
        if (this.props.fold)
            return null
        else {
            let height = this.props.height - 35;
            return (
                <div
                    className={"body"}
                    style={{height}}>
                        {this.header()}
                        {this.body()}
                </div>
            );
        }
        
    }
    header = ()=>{
        return (
            <div
                className= {`header`}>
                    {"Documentation"}
            </div>
        );
    }
    body = () => {
        let indexParent = [];
        return this.props.elements.map((element, index)=>(
            <MenuElement
                key = {`main-menu-element-${indexParent.concat([index])}`}
                index = {indexParent.concat([index])}
                element = {element}
                active = {this.props.active}
                hidden = {false}
                theme = {this.props.theme}
                onChange = {this.fromChild}
            />
        ));
    };
    fromChild = (e) => {
        if (e.active)
            this.props.onChange({active: e.active});
    }
    folder=()=>{
        if (!this.props.fold)
            return (
                <div
                    className={"fold"}
                    onClick={this.doFold}>
                        <FontAwesomeIcon icon={faChevronLeft}/>
                </div>
            );
        else return null;
    }
    doFold=()=>{
        this.props.onChange({fold:true});
    }
    expander=()=>{
        if (this.props.fold)
            return (
                <div
                    className={"expand"}
                    onClick={this.doExpand}>
                        <FontAwesomeIcon icon={faChevronRight}/>
                </div>
            );
        else return null;
    }
    doExpand=()=>{
        this.props.onChange({expand:true});
    }
    render() {
        let theme = this.props.theme ? "lightMode" : "darkMode";
        let width = this.props.width;
        return (
            <div
                className= {`main docs menu ${theme}`}
                style={{width}}>
                    
                    {this.menu()}
                    {this.folder()}
                    {this.expander()}
                    
            </div>
        );
    }
}

export default Menu;