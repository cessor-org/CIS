import React, { PureComponent } from "react";
import {
    faPlus,faChevronRight, faChevronDown
  } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
class MenuElement extends PureComponent {
    constructor(props) {
        super(props);
        this.state = {
            children : false,
            expand : false,
            active : false
        }
    }
    componentDidMount(){
        let element = this.props.element;
        
        let state = {};
        if (element.children && element.children.length>0) {
            state.children = true;
            let index = this.props.index;
            let active = this.props.active;
            if (index[index.length-1] == active[index.length-1])
                state.expand = true;
            else state.expand = false;
        }
        if (this.checkActivate())
            state.active = true;
        this.update(state);
    }
    componentDidUpdate(){
        if(!this.state.active) {
            if (this.checkActivate()) {
                this.props.onChange({expand : true});
                this.update({active : true});
            }
        }
    }
    expandIcon = () => {
        if (this.state.children) {
            if (this.state.expand) {
                return (
                    <div
                        className="icon"
                        onClick={()=>this.expand(false)}>
                        <FontAwesomeIcon icon={faChevronDown}/>
                    </div>
                );
            } else {
                return (
                    <div
                        className="icon"
                        onClick={()=>this.expand(true)}>
                        <FontAwesomeIcon icon={faChevronRight}/>
                    </div>
                );
            }
        } else return null;
    }
    expand = (event) => {
        if (event) {
            this.update({expand : true});
        } else {
            this.update({expand : false});
        }
    }
    click = () => {
        if (this.state.children) {
            if (this.state.expand) {
                this.update({expand : false});
            } else {
                this.update({expand : true});
            }
        } else {
            let element = this.props.element;
            this.props.onChange({active: this.props.index});
        }
    }
    children = () => {
        let indexParent = this.props.index;
        if (this.state.children) {
            let element = this.props.element;
            return element.children.map((element, index)=>(
                <MenuElement
                    key = {`main-menu-element-${indexParent.concat([index])}`}
                    index = {indexParent.concat([index])}
                    element = {element}
                    active = {this.props.active}
                    hidden = {!this.state.expand}
                    theme = {this.props.theme}
                    onChange = {this.fromChild}
                />
            ));
        } else {
            return null;
        }
    }
    fromChild = (e) => {
        if (e.active)
            this.props.onChange({active: e.active});
        if (e.expand) {
            this.props.onChange({expand : true});
            this.update({expand : true});
        }
    }
    checkActivate = () => {
        let index = this.props.index;
        let active = this.props.active;
        if (index.length == active.length) {
            let cntrlr = true;
            for (let i=0; i<index.length; i++) {
                if (index[i] != active[i])
                    cntrlr = false;
                if (i == index.length-1)
                    return cntrlr;
            }
        } else return false;
    }
    //  Style
    font = () => {
        let length = this.props.index.length;
        if (length == 1)
            return 16;
        else if (length == 2)
            return 15;
        else if (length == 3)
            return 14;
        else if (length == 4)
            return 13;
        else return 12;
    }
    margin = () => {
        let length = this.props.index.length;
        if (length == 1)
            return "5px 5px";
        else return "0 0 0 8px";
    }
    padding_dep = () => {
        let length = this.props.index.length;
        if (length == 1)
            return "10px 0";
        else if (length == 2)
            return "9px 0";
        else if (length == 3)
            return "8px 0";
        else if (length == 4)
            return "7px 0";
        else return "6px 0";
    }
    padding = () => {
        let length = this.props.index.length;
        if (length == 1)
            return "10px 0";
        else if (length == 2) {
            if (this.props.index[length-1]==0)
                return "18px 0 9px 0";
            else
                return "9px 0";
        }
        else if (length == 3)
            if (this.props.index[length-1]==0)
                return "16px 0 8px 0";
            else
                return "8px 0";
        else if (length == 4)
            if (this.props.index[length-1]==0)
                return "14px 0 7px 0";
            else
                return "7px 0";
        else
            if (this.props.index[length-1]==0)
                return "12px 0 6px 0";
            else
                return "6px 0";
    }
    border = () => {
        let length = this.props.index.length;
        if (length == 1)
            return "none";
        else {
            let theme = this.props.theme ? "lightMode" : "darkMode";
            return `solid 1px var(--${theme}-main-docs-menu-line)`;
        }
    }
    borderRadius = () => {
        let length = this.props.index.length;
        if (length == 1)
            return 5;
        else return 0;
    }
    update = (new_state) => {
        this.setState(new_state, function () {
          this.forceUpdate();
        });
    };
    render() {
        let element = this.props.element;
        let style = {
            display : this.props.hidden? "none" : "block",
            margin : this.margin(),
            padding : this.padding(),
            borderLeft : this.border(),
            borderRadius : this.borderRadius()
        };
        return (
            <div
                className={`menuElement
                            ${this.checkActivate() ? "active" : ""}`}
                style = {style}
                >
                    <div 
                        className="title"
                        style={{fontSize: this.font()}}
                        onClick={this.click}>
                        {element.title}
                    </div>
                    {this.expandIcon()}
                    {this.children()}
            </div>
        );
    }
}

export default MenuElement;