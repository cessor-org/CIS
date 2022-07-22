import React, { PureComponent } from "react";
import Menu from "./docs/menu.jsx";
import Body from "./docs/body.jsx";
import {cloneDeep} from "lodash";

class Docs extends PureComponent {
    constructor(props) {
        super(props);
        this.state = {
            active : [0],
            menu : {
                width : 200,
                fold : false
            },
            elements : require(`./docs/menu.json`),
            load :false
        }
    }
    componentDidMount(){
        this.update({load:true});
    }
    menu=()=>{
        return (
            <Menu
                theme = {this.props.theme}
                width = {this.state.menu.width}
                fold = {this.state.menu.fold}
                height = {this.props.height}
                active = {this.state.active}
                elements = {this.state.elements}
                onChange = {this.fromMenu}
            />);
    }
    fromMenu= (e)=>{
        let active = this.state.active;
        let menu = this.state.menu;
            
        if (e.active || e.active === 0){
            active = e.active;
        }
        if (e.fold) {
            menu.fold = true;
            menu.width = 35;
        }
        if (e.expand) {
            menu.fold = false;
            menu.width = 200;
        }
        this.update({menu, active});
    }
    body =()=>{
        let left = this.state.menu.width;
        let width = this.props.width - left;
        return (
            <Body
                theme = {this.props.theme}
                left = {left}
                width = {width}
                active = {this.state.active}
                elements = {this.state.elements}
                onChange = {this.fromBody}
            />
        );
    }
    fromBody= (e)=>{
        
        if (e.link) {
            let elements  = this.state.elements;
            let link = cloneDeep(e.link);
            let active = this.getIndexOfElement(link, elements, []);
            this.update({active});
        }
    }
    getIndexOfElement = (link, elements, index) => {
        let id = link.shift();
        let children;
        elements.forEach((element, i) => {
            if (element.id === id){
                index.push(i);
                children = element.children;
            }
        });
        if (link.length > 0) {
            return this.getIndexOfElement(link, children, index);
        }
        else return index;

    }
    update = (new_state) => {
        this.setState(new_state, function () {
          this.forceUpdate();
        });
    };
    render() {
        let load = this.state.load;
        let active = this.props.active;
        return load && active && (
            <div className="col-12">
                <div className={`row`} >
                    {this.menu()}
                    {this.body()}
                </div>
            </div>
        );
    }
}

export default Docs;